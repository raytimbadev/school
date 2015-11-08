#include "sfs_helpers.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct sfs_superblock * SUPERBLOCK = NULL;

int
sfs_format(int fresh, char *path, size_t block_size, size_t block_count)
{
    if(SUPERBLOCK != NULL)
    {
        fprintf(stderr, "Calling sfs_format twice.\n");
        return -1;
    }

    unsigned int inodes_per_block = block_size / sizeof(struct sfs_inode);
    unsigned int inode_count =
        block_size * block_count / SFS_INODE_ALLOC_HEURISTIC;
    unsigned int bits_per_block = block_size * 8;

    unsigned int inode_blocks = inode_count / inodes_per_block + 1;
    
    unsigned int data_bitmap_blocks = block_count / bits_per_block + 1;
    unsigned int inode_bitmap_blocks = inode_count / bits_per_block + 1;

    unsigned int total_blocks = 1 +
        block_count + inode_blocks +
        data_bitmap_blocks + inode_bitmap_blocks;

    // TODO check for errors
    if(fresh == SFS_FRESH)
        if(init_fresh_disk(path, block_size, total_blocks) != 0)
            return -1;
    else
        if(init_disk(path, block_size, total_blocks) != 0)
            return -1;

    SUPERBLOCK = malloc(sizeof(*SUPERBLOCK));
    SUPERBLOCK->magic = MAGIC;
    SUPERBLOCK->block_size = block_size;
    SUPERBLOCK->block_count = block_count;
    SUPERBLOCK->inode_blocks = inode_blocks;
    SUPERBLOCK->inode_bitmap_count = inode_bitmap_blocks;
    SUPERBLOCK->block_bitmap_count = data_bitmap_blocks;

    return 0;
}

struct sfs_superblock *
load_superblock()
{
    return SUPERBLOCK;
}

size_t
get_inodes_per_block(const struct sfs_superblock *superblock)
{
    return superblock->block_size / sizeof(struct sfs_inode);
}

size_t
get_inode_table_offset(const struct sfs_superblock *superblock)
{
    return SFS_SUPERBLOCK_COUNT;
}

size_t
get_data_blocks_offset(const struct sfs_superblock *superblock)
{
    return SFS_SUPERBLOCK_COUNT + superblock->inode_blocks;
}

size_t
get_inode_bitmap_offset(const struct sfs_superblock *superblock)
{
    return SFS_SUPERBLOCK_COUNT + superblock->inode_blocks + superblock->block_count;
}

size_t
get_block_bitmap_offset(const struct sfs_superblock *superblock)
{
    return SFS_SUPERBLOCK_COUNT +
        superblock->inode_blocks +
        superblock->block_count +
        superblock->inode_bitmap_count;
}

size_t
block_ceiling(const struct sfs_superblock *superblock, size_t bytes)
{
    return bytes / superblock->block_size + 1;
}

char *
block_malloc(const struct sfs_superblock *superblock, size_t block_count)
{
    return calloc(block_count, superblock->block_size);
}

/**
 * Determines what block a given inode is stored on.
 *
 * Returns 0 and sets the value pointed to by `m` to the block number holding
 * the desired inode.
 * Returns -1 in case of failure.
 */
int
resolve_inode_to_block(sfs_inode_n n, sfs_block_ptr *m, sfs_inode_n *p)
{
    const struct sfs_superblock *superblock = load_superblock();
    const size_t inodes_per_block = get_inodes_per_block(superblock);
    const size_t inode_block_offset = n / inodes_per_block;

    if(inode_block_offset + 1 >= superblock->inode_blocks)
    {
        fprintf(stderr, "inode number too big.\n");
        return -1;
    }

    if(p != NULL)
        *p = inode_block_offset * inodes_per_block;
    
    *m = get_inode_table_offset(superblock) + inode_block_offset;
    return 0;
}

int
load_inode(sfs_inode_n n, struct sfs_inode **inode)
{
    sfs_block_ptr inode_block; 
    sfs_inode_n base_inode_n;

    if(resolve_inode_to_block(n, &inode_block, &base_inode_n) != 0)
        return -1;
    
    const struct sfs_superblock *superblock = load_superblock();
    const sfs_inode_n inode_offset = n - base_inode_n;
    void *buf = block_malloc(superblock, 1);

    const int block_count = read_blocks(inode_block, 1, buf);

    if(block_count < 1)
    {
        // failed to read the block
        free(buf);
        return -1;
    }

    const struct sfs_inode *inodes = (const struct sfs_inode *)buf;

    if(*inode == NULL)
        *inode = malloc(sizeof(**inode));

    **inode = inodes[inode_offset];

    free(buf);
    return 0;
}

int
read_direct(
        const struct sfs_superblock *sb,
        const struct sfs_inode *inode,
        void *buf)
{
    unsigned int offset;
    int i;
    for(i = 0, offset = 0;
            i < SFS_DIRECT_PTR_COUNT;
            i++, offset += sb->block_size)
    {
        if(inode->direct_blocks[i] == 0)
            goto end;

        if(read_blocks(inode->direct_blocks[i], 1, buf + offset) < 1)
            return -1;
    }

    return offset;

end:
    return 0;
}

int
read_indirect(
        const struct sfs_superblock *sb,
        const struct sfs_inode *inode,
        void *buf)
{
    void *indirect_buf = block_malloc(sb, 1);
    const int read_blocks_ret =
        read_blocks(inode->indirect_block, 1, indirect_buf);

    if(read_blocks_ret < 1)
        return -1;

    const size_t ptrs_per_block = sizeof(sfs_block_ptr) / sb->block_size;
    const sfs_block_ptr *block_ptrs =
        (const sfs_block_ptr *)indirect_buf;

    unsigned int i, offset;
    for(i = 0, offset = 0;
            i < ptrs_per_block;
            i++, offset += sb->block_size)
    {
        if(block_ptrs[i] == 0)
            goto end;

        if(read_blocks(block_ptrs[i], 1, buf + offset) < 1)
            goto fail;
    }

    free(indirect_buf);
    return offset;

end:
    free(indirect_buf);
    return 0;

fail:
    free(indirect_buf);
    return -1;
}

int
read_whole_file(const struct sfs_inode *inode, void **buf)
{
    const struct sfs_superblock *sb = load_superblock();
    const size_t block_count = block_ceiling(sb, inode->size);
    
    if(*buf == NULL)
        *buf = block_malloc(sb, block_count);
    else
        memset(*buf, 0, block_count * sb->block_size);

    unsigned int offset;

    if((offset = read_direct(sb, inode, *buf)) == 0)
        goto end;
    else if(offset == -1)
        return -1;

    if(inode->indirect_block == 0)
        goto end;

    return read_indirect(sb, inode, *buf + offset + sb->block_size);

end:
    return 0;
}

int
follow_path(
        char *path,
        struct sfs_inode **inode,
        struct sfs_inode *parent)
{
    int ret;
    char *path_p = path;

    if(path[0] == '/')
    {
        const struct sfs_superblock *sb = load_superblock();

        if(parent != NULL)
            return -1;

        if(load_inode(sb->root, &parent) != 0)
            return -2;

        goto recurse;
    }

    if(parent == NULL)
        return -1;

    // seek with another pointer to the next slash or till the end of the
    // string
    while(*path_p != '/' && *path_p != '\0')
        path_p++;

    // if we've hit the end of the string, set the done flag
    int done = *path_p == '\0';
    *path_p = '\0';

    // now path is just the name of leftmost path element

    struct sfs_dir_iter *dir = NULL;
    if(listdir(parent, &dir) != 0)
    {
        free(dir);
        return -3;
    }

    const sfs_inode_n child_inode = dirlookup(dir, path);

    if(child_inode == SFS_INODE_NULL)
        return -4;

    if(load_inode(child_inode, &parent) != 0)
        return -5;

    if(!done)
        goto recurse;

    // if we're done then we can allocate the inode struct and copy the parent
    // into it (since we stored the last loaded inode in parent)
    if(*inode == NULL)
        *inode = malloc(sizeof(**inode));

    **inode = *parent;
    return 0;

recurse:
    ret = follow_path(path_p + 1, inode, parent);
    free(parent);

    if(ret < 0)
        return ret;
    else
        return ret + 1;
}

sfs_inode_n
dirlookup(const struct sfs_dir_iter *dir, const char *filename)
{
    unsigned int i;
    for(i = 0; i < dir->size; i++)
    {
        const char *other_filename = dir->entries[i].filename;
        if(strncmp(filename, other_filename, MAXFILENAME) == 0)
            return dir->entries[i].inode;
    }

    return SFS_INODE_NULL;
}

int
listdir(const struct sfs_inode *inode, struct sfs_dir_iter **iter)
{
    if(*iter == NULL)
        *iter = malloc(sizeof(*iter));

    if(inode->mode & MODE_D == 0)
        return -1; // not a directory

    // read in the directory directory
    void *buf = NULL;
    if(read_whole_file(inode, &buf) != 0)
    {
        free(buf);
        return -3;
    }
    
    const size_t file_count = inode->size / sizeof(struct sfs_dir_entry);
    const struct sfs_dir_entry *entries =
        (const struct sfs_dir_entry *)buf;
    
    (*iter)->entries = entries;
    (*iter)->size = file_count;
    (*iter)->position = 0;

    return 0;
}
