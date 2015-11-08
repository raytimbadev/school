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
get_max_file_size(const struct sfs_superblock *sb)
{
    // the block size multiplied by the number of direct pointers plus the
    // maximum number of pointers storable in one block.
    return sb->block_size *
        (SFS_DIRECT_PTR_COUNT + sb->block_size / sizeof(sfs_block_ptr));
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
read_whole_file(const struct sfs_inode *inode, void **buf)
{
    if(*buf == NULL)
        *buf = calloc(inode->size, sizeof(char));
    else
        memset(*buf, 0, inode->size);

    return basic_read(inode, *buf, inode->size, 0);
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
        free_dir_iter(dir);
        return -3;
    }

    const sfs_inode_n child_inode = dirlookup(dir, path);
    free_dir_iter(dir);

    if(child_inode == SFS_INODE_NULL)
        return -4;

    if(load_inode(child_inode, &parent) != 0)
        return -5;

    if(!done)
        goto recurse;

    // if we're done then the parent pointer in fact points to the file
    // identified by the full path. We can simply set *inode to parent then
    // to return this inode struct to the top caller.

    *inode = parent;

    return 0;

recurse:
    // path_p points to the '/' that we changed to a null, so the next
    // character is the beginning of the next path element.
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
    if(*iter == SFS_DIR_START)
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
    struct sfs_dir_entry *entries =
        (struct sfs_dir_entry *)buf;

    (*iter)->entries = entries;
    (*iter)->size = file_count;
    (*iter)->position = 0;

    return 0;
}

void
free_dir_iter(struct sfs_dir_iter *iter)
{
    free(iter->entries);
    free(iter);
}

int
file_open(const char *path, struct sfs_file **file)
{
    struct sfs_inode *inode = NULL;
    char *mpath = strdup(path);

    if(follow_path(mpath, &inode, NULL) != 0)
    {
        free(mpath);
        return -1;
    }

    if(*file == NULL)
        *file = malloc(sizeof(**file));

    **file = (struct sfs_file) {
        .inode = inode,
        .file_offset = 0,
        .buf_offset = 0,
        .buf = calloc(SFS_WRITEBUF_SIZE, sizeof(char)),
        .buf_size = SFS_WRITEBUF_SIZE
    };

    free(mpath);
    return 0;
}

int
file_buffer_resize(struct sfs_file *file, size_t new_size)
{
    int ret = 0;
    void *newbuf = NULL;

    if(new_size < file->buf_offset)
        if((ret = file_flush(file)) < 0)
            return -2;

    // allocate a new buffer with the desired size
    newbuf = calloc(new_size, sizeof(char));

    // copy the existing buffer's data into it
    memcpy(newbuf, file->buf, file->buf_offset);

    // free the old buffer
    free(file->buf);

    // set the new buffer as the file's write-buffer
    file->buf = newbuf;

    // return the number of flushed bytes, if any.
    return ret;
}

int
basic_read(
        const struct sfs_inode *inode,
        void *dst,
        size_t size,
        unsigned int src_offset)
{
    // the offset within the destination buffer to copy data
    unsigned int dst_offset = 0;

    // the remaining number of bytes to read
    unsigned int remaining = size;

    // the superblock
    const struct sfs_superblock *sb = load_superblock();

    // the offset of the first data block on disk.
    const unsigned int db_offset = get_data_blocks_offset(sb);

    // the index of first pointer within the full array of data block pointers
    // in the inode.
    const unsigned int start_block_n = src_offset / sb->block_size;

    // adjust the source offset to be within the first block.
    src_offset -= start_block_n * sb->block_size;

    // one plus the index of the last pointer within the full array of data
    // block pointers in the inode.
    const unsigned int end_block_n = (src_offset + size) / sb->block_size;

    // represents whether indirect blocks will be needed
    const char need_indirect = end_block_n > SFS_DIRECT_PTR_COUNT;

    // allocate a block-sized buffer to hold blocks read from disk.
    void *block_buf = calloc(sb->block_size, sizeof(char));

    // the block pointer index and the number of bytes to copy from the current
    // block (recalculated for each new block read)
    int i = start_block_n, copy_count;

    // read direct blocks, if any. This loop will be skipped if start_block_n
    // turns out to be greater than the number of direct pointers held in an
    // inode.
    for(i = start_block_n;
            i < end_block_n &&
            i < SFS_DIRECT_PTR_COUNT &&
            remaining > 0;

            i++,
            dst_offset += copy_count,
            src_offset = 0,
            remaining -= copy_count)
    {
        // read the next block into the block buffer
        if(read_blocks(db_offset + inode->direct_blocks[i], 1, block_buf) < 1)
        {
            free(block_buf);
            return -1;
        }

        // compute the number of bytes in the block past the source offset
        copy_count = sb->block_size - src_offset;

        // if that number of bytes is less than the number of remaining bytes
        // to read, then the number of bytes to copy is simply the number of
        // remaining bytes.
        if(remaining < copy_count)
            copy_count = remaining;

        // copy that number of bytes from the block buffer to the destination
        memcpy(dst + dst_offset, block_buf + src_offset, copy_count);
    }

    // if no indirect blocks are needed, then we can simply return immediately.
    if(!need_indirect)
    {
        free(block_buf);
        return (signed int)size;
    }

    // otherwise, we read the indirect pointer block

    const unsigned int block_ptrs_per_block =
        sb->block_size / sizeof(sfs_block_ptr);
    sfs_block_ptr indirect_ptrs[block_ptrs_per_block];
    if(read_blocks(
                db_offset + inode->indirect_block,
                1,
                (void*)indirect_ptrs) < 0)
    {
        free(block_buf);
        return -1;
    }

    for(i;
            i < end_block_n &&
            i < block_ptrs_per_block &&
            indirect_ptrs[i] != SFS_NULL &&
            remaining > 0;

            i++,
            dst_offset += copy_count,
            src_offset = 0,
            remaining -= copy_count)
    {
        // read the next block into the block buffer
        if(read_blocks(db_offset + indirect_ptrs[i], 1, block_buf) < 1)
        {
            free(block_buf);
            return -1;
        }

        // compute the number of bytes in the block past the source offset
        copy_count = sb->block_size - src_offset;

        // if that number of bytes is less than the number of remaining bytes
        // to read, then the number of bytes to copy is simply the number of
        // remaining bytes.
        if(remaining < copy_count)
            copy_count = remaining;

        // copy that number of bytes from the block buffer to the destination
        memcpy(dst + dst_offset, block_buf + src_offset, copy_count);
    }

    free(block_buf);
    return size;
}

int
file_read(struct sfs_file *file, void *dst, size_t size)
{
    // flush any write buffers on the file
    if(file_flush(file) < 0)
        return -2;

    // if we're at the end of the file, do nothing.
    if(file_eof(file))
        return 0;

    // since basic_read reads *exactly* the number of bytes it's told, whereas
    // file_read reads *at most* the number of bytes it's told, we have to
    // adjust the size to be at most the maximum number of bytes that can still
    // be read, if the requested size is too large.
    if(file->file_offset + size > file->inode->size)
        size = file->inode->size - file->file_offset;

    // perform the read
    const int ret = basic_read(file->inode, dst, size, file->file_offset);

    // advance the file offset if successful
    if(ret > 0)
        file->file_offset += ret;

    // return the number of bytes read.
    return ret;
}

int
file_write(struct sfs_file *file, void *buf, size_t size)
{
    if(size > file->buf_size)
        return -1;

    int ret = 0;

    if(size + file->buf_offset > file->buf_size)
        if((ret = file_flush(file)) < 0)
            return -2;

    memcpy(file->buf + file->buf_offset, buf, size);
    file->buf_offset += size;
    return ret;
}

int
file_seek(struct sfs_file *file, unsigned int offset)
{
    if(file_flush(file) < 0)
        return -2;

    if(offset > file->inode->size)
        return -1;

    file->file_offset = offset;
}

int
file_flush(struct sfs_file *file)
{
    fprintf(stderr, "STUB: file_flush\n");
    return -1;
}

int
file_close(struct sfs_file *file)
{
    if(file_flush(file))
        return -2;

    free(file->buf);
    free(file->inode);
    free(file);
    return 0;
}

int
file_eof(struct sfs_file *file)
{
    return file->file_offset == file->inode->size;
}
