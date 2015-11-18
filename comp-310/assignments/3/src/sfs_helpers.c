#define _GNU_SOURCE

#include "sfs_helpers.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct sfs_superblock * SUPERBLOCK = NULL;

struct sfs_superblock *
create_superblock(size_t block_size, size_t block_count)
{
    unsigned int bitpacks = block_count / BITPACK_SIZE + 1;
    block_count = bitpacks * BITPACK_SIZE;
    fprintf(stderr,
            "SFS: create_superblock: rounded block_count to %zu.\n",
            block_count);

    unsigned int inodes_per_block = block_size / sizeof(struct sfs_inode);
    unsigned int inode_count =
        block_size * block_count
        / SFS_INODE_ALLOC_HEURISTIC
        / sizeof(struct sfs_inode);

    fprintf(stderr,
            "SFS: create_superblock: computed inode count %u.\n",
            inode_count);

    inode_count = inode_count / BITPACK_SIZE + 1;
    inode_count *= BITPACK_SIZE;

    fprintf(stderr,
            "SFS: create_superblock: rounded inode count to %u.\n",
            inode_count);

    unsigned int bits_per_block = block_size * CHAR_BIT;

    unsigned int inode_blocks = inode_count / inodes_per_block + 1;

    fprintf(stderr,
            "SFS: create_superblock: %u blocks required to house inodes.\n",
            inode_blocks);

    unsigned int data_bitmap_blocks = block_count / bits_per_block + 1;
    unsigned int inode_bitmap_blocks = inode_count / bits_per_block + 1;

    fprintf(stderr,
            "SFS: create_superblock: %u blocks for data bitmap.\n",
            data_bitmap_blocks);

    fprintf(stderr,
            "SFS: create_superblock: %u blocks for inode bitmap.\n",
            inode_bitmap_blocks);

    struct sfs_superblock *sb = malloc(sizeof(*sb));
    sb->magic = MAGIC;
    sb->block_size = block_size;
    sb->block_count = block_count;
    sb->inode_count = inode_count;
    sb->inode_blocks = inode_blocks;
    sb->inode_bitmap_count = inode_bitmap_blocks;
    sb->block_bitmap_count = data_bitmap_blocks;
    sb->root = 0;

    fprintf(stderr,
            "SFS: create_superblock: total disk size: %zu blocks.\n",
            get_total_block_count(sb));

    return sb;
}

/**
 * Persists the core data structures to disk.
 *
 * Must be called after a fresh disk has been created and after the superblock
 * has been allocated in memory.
 */
void
sfs_init_fresh()
{
    struct sfs_superblock *sb = load_superblock();

    // persist the superblock
    void *sb_buf = calloc(1, sb->block_size * sizeof(char));
    *(struct sfs_superblock *)sb_buf = *sb;
    write_blocks(0, 1, sb_buf);
    free(sb_buf);

    // allocate a new inode structure for the root directory
    struct sfs_inode *root_inode = new_inode(0, SFS_DEFAULT_DIR_MODE);

    // persist the root inode to disk.
    if(inode_persist(sb, root_inode) < 0)
        fprintf(stderr,
                "SFS: sfs_init: failed to persist root directory inode.\n");

    // Now we have to mark root inode as used.
    // load the inode bitfield from disk.
    struct free_bitfield *inode_bitfield = load_free_inodes_bitfield();

    // mark the root inode as used.
    bitfield_mark(inode_bitfield, &sb->root, 1, BIT_USED);

    // persist the bitfield to disk.
    bitfield_persist(
            sb,
            inode_bitfield,
            get_inode_bitmap_offset(sb));

    // free the in-memory inode structure for the root directory
    free(root_inode);
    free_bitfield(inode_bitfield);
}

int
sfs_init(int fresh, char *path, size_t block_size, size_t block_count)
{
    fprintf(stderr, "SFS: sfs_init.\n");

    if(SUPERBLOCK != NULL)
    {
        // TODO check out reinitialization logic?
        fprintf(stderr, "SFS: sfs_init: formatting twice.\n");
        return -1;
    }

    if(fresh == SFS_FRESH)
    {
        // allocate the superblock in memory.

        SUPERBLOCK = create_superblock(block_size, block_count);

        // initialize a fresh disk big enough to hold the filesystem
        if(init_fresh_disk(
                    path,
                    block_size,
                    get_total_block_count(SUPERBLOCK)) != 0)
            return -1;

        // persist the core data structures to disk.
        sfs_init_fresh();
    }
    else
    {
        // pretend the disk has just one block, so that we can load the
        // superblock
        if(init_disk(path, block_size, 1) != 0)
            return -1;

        // load the superblock from disk.
        SUPERBLOCK = load_superblock_from_disk(block_size);

        // reinitialize the disk with the new parameters.
        if(init_disk(
                    path,
                    SUPERBLOCK->block_size,
                    get_total_block_count(SUPERBLOCK)) != 0)
            return -1;
    }

    return 0;
}

struct sfs_superblock *
load_superblock_from_disk(size_t block_size)
{
    void *sb_buf = calloc(1, block_size * sizeof(char));
    read_blocks(0, 1, sb_buf);
    struct sfs_superblock *sb = malloc(sizeof(*sb));
    *sb = *(struct sfs_superblock *)sb_buf;
    free(sb_buf);
    return sb;
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
get_total_block_count(const struct sfs_superblock *sb)
{
    return SFS_SUPERBLOCK_COUNT +
        sb->inode_blocks +
        sb->block_count +
        sb->inode_bitmap_count +
        sb->block_bitmap_count;
}

size_t
block_ceiling(const struct sfs_superblock *superblock, size_t bytes)
{
    size_t blocks = bytes / superblock->block_size;
    if(bytes % superblock->block_size != 0)
        blocks++;
    return blocks;
}

void *
block_malloc(const struct sfs_superblock *superblock, size_t block_count)
{
    return calloc(block_count, superblock->block_size);
}

/**
 * Determines what block within the inode table a given inode is stored on.
 *
 * In case of success, returns 0 and sets the value pointed to by `m` to the
 * block number holding the desired inode, and sets the value pointed to by `p`
 * to the inode number of the first inode stored on that block.
 *
 * Returns -1 in case of failure.
 */
int
resolve_inode_to_block(sfs_inode_n n, sfs_block_ptr *m, sfs_inode_n *p)
{
    const struct sfs_superblock *superblock = load_superblock();
    const size_t inodes_per_block = get_inodes_per_block(superblock);
    const size_t inode_block_offset = n / inodes_per_block;

    if(n >= superblock->inode_count)
    {
        fprintf(stderr, "SFS: resolve_inode_to_block: inode number too big.\n");
        return -1;
    }

    if(p != NULL)
        *p = inode_block_offset * inodes_per_block;

    *m = get_inode_table_offset(superblock) + inode_block_offset;

    fprintf(stderr,
            "SFS: resolve_inode_to_block: inode %u -> block %u on disk.\n",
            n,
            *m);

    return 0;
}

int
load_inode(sfs_inode_n n, struct sfs_inode **inode)
{
    sfs_block_ptr inode_block;
    sfs_inode_n base_inode_n;

    if(resolve_inode_to_block(n, &inode_block, &base_inode_n) != 0)
    {
        fprintf(stderr,
                "SFS: load_inode: fail: could not resolve inode %d to a "
                "block.\n",
                n);
        return -1;
    }

    const struct sfs_superblock *superblock = load_superblock();
    const sfs_inode_n inode_offset = n - base_inode_n;
    void *buf = block_malloc(superblock, 1);

    const int block_count = read_blocks(inode_block, 1, buf);

    if(block_count < 1)
    {
        // failed to read the block
        fprintf(stderr,
                "SFS: load_inode: fail: could not read the block holding "
                "inode %d.\n",
                n);
        free(buf);
        return -1;
    }

    const struct sfs_inode *inodes = (const struct sfs_inode *)buf;

    if(*inode == NULL)
    {
        fprintf(stderr,
                "SFS: load_inode: automatically allocating inode struct.\n");
        *inode = malloc(sizeof(**inode));
    }
    else
        fprintf(stderr,
                "SFS: load_inode: overwriting existing inode struct.\n");

    **inode = inodes[inode_offset];

    // sanity check
    if(inodes[inode_offset].n == n)
        fprintf(stderr,
                "SFS: load_inode: success: loaded inode number %u.\n", n);
    else
        fprintf(stderr,
                "SFS: load_inode: warn: loaded inode's number does not match "
                "requested inode number.\n");

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
        struct sfs_inode *parent,
        const sfs_mode mode)
{
    int ret;
    char *path_p = path;

    // this happens if the path ends with a slash
    // empty string is just another name for root, amirite?
    if(strlen(path) == 0)
    {
        if(parent == NULL)
        {
            const struct sfs_superblock *sb = load_superblock();
            if(load_inode(sb->root, inode) != 0)
                return -2;
            return 0;
        }

        *inode = malloc(sizeof(**inode));
        **inode = *parent;
        return 0;
    }

    if(path[0] == '/')
    {
        const struct sfs_superblock *sb = load_superblock();

        if(parent != NULL)
            return -1;

        if(load_inode(sb->root, &parent) != 0)
            return -2;

        fprintf(stderr, "SFS: follow_path: recursing from root directory.\n");
        goto recurse;
    }

    if(parent == NULL)
    {
        fprintf(stderr, "SFS: follow_path: fail: no parent for non-root.\n");
        return -1;
    }

    // seek with another pointer to the next slash or till the end of the
    // string
    while(*path_p != '/' && *path_p != '\0')
        path_p++;

    // if we've hit the end of the string, set the done flag
    int done = *path_p == '\0';
    *path_p = '\0';

    if(strlen(path) > MAXFILENAME)
    {
        fprintf(stderr,
                "SFS: follow_path: path component '%s' too long. "
                "Maximum size: %d\n",
                path,
                MAXFILENAME);
        return -1;
    }

    if(!done)
        fprintf(stderr,
                "SFS: follow_path: path split '%s', '%s'\n",
                path,
                path_p + 1);

    // now path is just the name of leftmost path element

    struct sfs_dir_iter *dir = NULL;
    if(listdir(parent, &dir) != 0)
    {
        fprintf(stderr,
                "SFS: follow_path: failed to listdir inode %d.\n",
                parent->n);
        return -3;
    }

    sfs_inode_n child_inode = dirlookup(dir, path);
    free_dir_iter(dir);

    // if the child doesn't exist
    if(child_inode == SFS_INODE_NULL)
    {
        fprintf(stderr,
                "SFS: follow_path: child does not exist.\n");
        // and if we've reached the end of the path and we have a mode, then we
        // need to create a new file.
        if(done && mode != SFS_NO_MODE)
            goto create;
        // otherwise, if we're not done or if we don't have a mode, then this
        // is a file not found error.
        else
        {
            fprintf(stderr,
                    "SFS: follow_path: fail: file not found!\n");
            return -4;
        }
    }
    else
        fprintf(stderr,
                "SFS: follow_path: child exists with inode number %d.\n",
                child_inode);

    parent = NULL;
    if(load_inode(child_inode, &parent) != 0)
    {
        fprintf(stderr,
                "SFS: follow_path: failed to load child inode %d.\n",
                child_inode);
        return -5;
    }

    if(!done)
        goto recurse;

    fprintf(stderr,
            "SFS: follow_path: reached end of path at file with inode %u.\n",
            parent->n);

    // if we're done then the parent pointer in fact points to the file
    // identified by the full path. We can simply set *inode to parent then
    // to return this inode struct to the top caller.

    *inode = parent;

    return 0;

create:
    fprintf(stderr,
            "SFS: follow_path: creating new file for missing child.\n");
    // allocate a new inode number
    if((child_inode = ialloc()) == SFS_INODE_NULL)
    {
        fprintf(stderr,
                "SFS: follow_path: fail: cannot allocate an inode for "
                "child.\n");
        return -6;
    }

    free(*inode);
    *inode = new_inode(child_inode, mode);

    // write the inode to disk.
    if(inode_persist(load_superblock(), *inode) < 0)
        fprintf(stderr,
                "SFS: follow_path: failed to persist inode %u.\n",
                child_inode);

    struct sfs_dir_entry entry = {
        .inode = child_inode
    };
    strncpy(entry.filename, path, MAXFILENAME);

    link_create(parent, &entry);

    return 0;

recurse:
    // path_p points to the '/' that we changed to a null, so the next
    // character is the beginning of the next path element.
    ret = follow_path(path_p + 1, inode, parent, mode);
    free(parent);

    if(ret < 0)
        return ret;
    else
        return ret + 1;
}

sfs_inode_n
dirlookup(const struct sfs_dir_iter *dir, const char *filename)
{
    fprintf(stderr,
            "SFS: dirlookup: looking up '%s'.\n",
            filename);

    unsigned int i;
    for(i = 0; i < dir->size; i++)
    {
        const char *other_filename = dir->entries[i].filename;
        if(strncmp(filename, other_filename, MAXFILENAME) == 0)
        {
            fprintf(stderr,
                    "SFS: dirlookup: HIT '%s' -> inode %u.\n",
                    filename,
                    dir->entries[i].inode);
            return dir->entries[i].inode;
        }
        else
            fprintf(stderr,
                    "SFS: dirlookup: MISS '%s' /= '%s'.\n",
                    filename,
                    dir->entries[i].filename);
    }

    return SFS_INODE_NULL;
}

int
listdir(const struct sfs_inode *inode, struct sfs_dir_iter **iter)
{
    if(*iter == SFS_DIR_START)
        *iter = malloc(sizeof(*iter));

    // check that the file is a directory
    if((inode->mode & MODE_D) == 0)
    {
        fprintf(stderr,
                "SFS: listdir: fail: not a directory (inode %u).\n",
                inode->n);
        free(*iter);
        return -1; // not a directory
    }

    // read in the directory directory
    void *buf = NULL;
    if(read_whole_file(inode, &buf) < 0)
    {
        fprintf(stderr,
                "SFS: listdir: fail: cannot load directory contents "
                "(inode %u).\n",
                inode->n);
        free(buf);
        return -3;
    }

    const size_t file_count = inode->size / sizeof(struct sfs_dir_entry);
    struct sfs_dir_entry *entries =
        (struct sfs_dir_entry *)buf;

    (*iter)->entries = entries;
    (*iter)->size = file_count;
    (*iter)->position = 0;

    fprintf(stderr,
            "SFS: listdir: loaded directory inode %u (%zu entries).\n",
            inode->n,
            file_count);

    return 0;
}

void
free_dir_iter(struct sfs_dir_iter *iter)
{
    free(iter->entries);
    free(iter);
}

int
file_open(
        const struct sfs_inode *inode,
        struct sfs_file **file)
{
    if(*file == NULL)
        *file = malloc(sizeof(**file));

    **file = (struct sfs_file) {
        .inode = *inode,
        .file_offset = inode->size,
        .buf_offset = 0,
        .buf = calloc(SFS_WRITEBUF_SIZE, sizeof(char)),
        .buf_size = SFS_WRITEBUF_SIZE
    };

    fprintf(stderr,
            "SFS: file_open: opened file; inode %u; size %zu\n",
            inode->n,
            inode->size);

    return 0;
}

int link_remove(
        const struct sfs_inode *dir_inode,
        const char *filename)
{
    struct sfs_file *file = NULL;

    // open the directory
    if(file_open(dir_inode, &file) != 0)
    {
        fprintf(stderr,
                "SFS: link_remove: failed to open directory (inode %u).\n",
                dir_inode->n);
        return -1;
    }

    file_seek(file, 0, SFS_START);

    // marker for the position in the directory where we found a match
    unsigned int found_at = 0;
    char found = 0;
    struct sfs_dir_entry entry;
    int read_count;
    sfs_inode_n found_inode;
    size_t initial_size = file->inode.size;

    fprintf(stderr,
            "SFS: link_remove: scanning directory (inode %u) for '%s'.\n",
            dir_inode->n,
            filename);

    // scan the directory, looking for files with the name we're looking for
    while(!file_eof(file))
    {
        read_count = file_read(file, &entry, sizeof(entry));

        if(read_count < sizeof(entry))
        {
            fprintf(stderr,
                    "SFS: link_remove: directory (inode %u) abruptly ended.\n",
                    dir_inode->n);
            break;
        }

        if(strncmp(filename, entry.filename, MAXFILENAME) != 0)
        {
            fprintf(stderr,
                    "SFS: link_remove: '%s' /= '%s'.\n",
                    filename,
                    entry.filename);
            continue;
        }

        found = 1;
        found_at = file_seek(file, 0, SFS_HERE) - sizeof(entry);
        found_inode = entry.inode;

        fprintf(stderr,
                "SFS: link_remove: found link to remove from directory "
                "(inode %u) at offset %u.\n",
                dir_inode->n,
                found_at);
    }

    // no link to remove
    if(!found)
    {
        file_close(file);
        fprintf(stderr,
                "SFS: link_remove: no link to remove.\n");
        return 0;
    }

    const size_t remaining = initial_size - found_at + sizeof(entry);

    void *buf = calloc(remaining, sizeof(char));

    fprintf(stderr,
            "SFS: link_remove: moving subsequent %zu bytes back by %zu "
            "bytes.\n",
            remaining,
            sizeof(entry));

    file_seek(file, found_at + sizeof(entry), SFS_START);
    read_count = file_read(file, buf, remaining);
    file_seek(file, found_at, SFS_START);
    file_write(file, buf, read_count);

    // shrink the file to release the space taken by the now deleted entry
    size_t new_size = found_at + read_count;
    file_truncate(file, new_size);

    // print the new directory listing
    file_seek(file, 0, SFS_START);

    fprintf(stderr, "SFS: link_remove: new directory listing:\n");
    while(!file_eof(file))
    {
        read_count = file_read(file, &entry, sizeof(entry));

        if(read_count < sizeof(entry))
        {
            fprintf(stderr,
                    "SFS: link_remove: directory (inode %u) abruptly ended.\n",
                    dir_inode->n);
            break;
        }

        fprintf(stderr,
                "SFS: link_remove: '%s' -> %u\n",
                entry.filename,
                entry.inode);
    }

    // we've now eliminated the old file entry by moving the subsequent entries
    // forward.
    file_close(file);

    // now we need to unlink the old inode
    struct sfs_inode *old_inode = NULL;

    // load the old inode and decrement its link count. If it still has files
    // pointing to it, then we can persist the modified inode and return 1 (the
    // number of files that we've unlinked)
    load_inode(found_inode, &old_inode);
    old_inode->link_count--;
    if(old_inode->link_count > 0)
    {
        inode_persist(load_superblock(), old_inode);
        fprintf(stderr,
                "SFS: link_remove: unlinked file (inode %u) still has %u "
                "links.\n",
                old_inode->n,
                old_inode->link_count);
        free(old_inode);
        return 1;
    }

    // otherwise, the link_count is zero! Meaning that we need to properly
    // delete the file.
    fprintf(stderr,
            "SFS: link_remove: unlinked file (inode %u) must be garbage "
            "collected.\n",
            old_inode->n);

    // Reset the file pointer to NULL, and open the file identified by the
    // old inode.
    file = NULL;
    file_open(old_inode, &file);

    // Set the file size to zero. This will free all the data blocks associated
    // with the file.
    file_truncate(file, 0);

    // close the file, now that it's data blocks have been released
    file_close(file);

    // mark the inode as free
    ifree(&old_inode->n, 1);
    free(old_inode);

    // return the number of unlinked files.
    return 1;
}

int
link_create(
        const struct sfs_inode *dir_inode,
        const struct sfs_dir_entry *entry)
{
    fprintf(stderr,
            "SFS: link_create: trying to link '%s' -> %d in directory inode "
            "%d.\n",
            entry->filename,
            entry->inode,
            dir_inode->n);

    struct sfs_file *file = NULL;

    // remove any existing link to the file we want to create here
    if(link_remove(dir_inode, entry->filename) < 0)
    {
        fprintf(stderr,
                "SFS: link_create: failed to remove existing links to %s in "
                "directory inode %u.\n",
                entry->filename,
                dir_inode->n);
        return -1;
    }

    // open the directory
    if(file_open(dir_inode, &file) != 0)
    {
        fprintf(stderr,
                "SFS: link_create: failed to open parent (inode %u).\n",
                dir_inode->n);
        return -1;
    }

    // write our new directory entry to the end of the file.
    if(file_write(file, entry, sizeof(*entry)) < 0)
        goto fail;

    // close the file.
    if(file_close(file) < 0)
    {
        fprintf(stderr,
                "SFS: link_create: failed to close directory (inode %u).\n",
                dir_inode->n);
        goto fail;
    }

    fprintf(stderr,
            "SFS: link_create: linked '%s' -> %u in directory inode %u.\n",
            entry->filename,
            entry->inode,
            dir_inode->n);

    return 0;

fail:
    free(file);
    return -1;
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
basic_write(
        const struct sfs_inode *inode,
        void *src,
        size_t size,
        unsigned int dst_offset)
{
    // the offset within the source buffer from which to copy data
    unsigned int src_offset = 0;

    // the remaining number of bytes to write
    unsigned int remaining = size;

    const struct sfs_superblock *sb = load_superblock();

    // the offset of the first data block on disk.
    const unsigned int db_offset = get_data_blocks_offset(sb);

    // the index of the first pointer within the full array of data block
    // pointers in the inode
    const unsigned int start_block_n = dst_offset / sb->block_size;

    const unsigned int end_block_n_maybe =
        (dst_offset + size)
        / sb->block_size;

    // the number of blocks that the write spans
    const unsigned int end_block_n = end_block_n_maybe +
        ((dst_offset + size) % sb->block_size == 0 ? 0 : 1);

    // adjust the destination offset to fall within a block.
    dst_offset -= start_block_n * sb->block_size;

    fprintf(stderr,
            "SFS: basic_write: writing %zu bytes, from data block %u up to "
            "%u, with first offset %u.\n",
            size,
            start_block_n,
            end_block_n,
            dst_offset);

    const char need_indirect = end_block_n > SFS_DIRECT_PTR_COUNT;

    void *block_buf = calloc(sb->block_size, sizeof(char));

    int i = start_block_n, copy_count;

    // allocate some space for indirect pointers later
    const unsigned int block_ptrs_per_block =
        sb->block_size / sizeof(sfs_block_ptr);
    sfs_block_ptr indirect_ptrs[block_ptrs_per_block];

    for(i = start_block_n;

            i < end_block_n &&
            i < SFS_DIRECT_PTR_COUNT &&
            remaining > 0;

            i++,
            dst_offset = 0,
            src_offset += copy_count,
            remaining -= copy_count)
    {
        fprintf(stderr,
                "SFS: basic_write: %u remaining bytes to write (i = %u).\n",
                remaining,
                i);

        // allocate a block-sized buffer
        memset(block_buf, 0, sb->block_size);

        // read any existing disk data into the block buffer
        if(read_blocks(
                    db_offset + inode->direct_blocks[i],
                    1,
                    block_buf) < 1)
        {
            fprintf(stderr,
                    "SFS: basic_write: failed to read existing data from "
                    "direct block %d.\n",
                    i);
            goto fail;
        }

        // the number of bytes to copy is the size of a block minus the number
        // of bytes to skip at the beginning (this only matters for the first
        // block to write)
        copy_count = sb->block_size - dst_offset;

        // if that number of bytes is less than the number of remaining bytes
        // to write, then the number of bytes to copy is simply the number of
        // remaining bytes.
        if(remaining < copy_count)
            copy_count = remaining;

        // copy from the source buffer into the block buffer, overwriting disk
        // data where expected
        memcpy(block_buf + dst_offset, src + src_offset, copy_count);

        fprintf(stderr,
                "SFS: basic_write: copied %u bytes from source buffer "
                "(offset %u) to block buffer (offset %u).\n",
                copy_count,
                src_offset,
                dst_offset);

        // write the block back to disk
        if(write_blocks(
                    db_offset + inode->direct_blocks[i],
                    1,
                    block_buf) < 1)
        {
            fprintf(stderr,
                    "SFS: basic_write: failed to write to direct block %d.\n",
                    i);
            goto fail;
        }

        fprintf(stderr,
                "SFS: basic_write: persisted direct block %d (0x%04x).\n",
                i,
                inode->direct_blocks[i]);
    }

    // if no indirect blocks are needed, then we can simply return immediately.
    if(!need_indirect)
    {
        fprintf(stderr,
                "SFS: basic_write: no indirect pointers, so returning "
                "early.\n");
        free(block_buf);
        return (signed int)size;
    }

    if(read_blocks(
                db_offset + inode->indirect_block,
                1,
                (void*)indirect_ptrs) < 0)
    {
        fprintf(stderr,
                "SFS: basic_write: failed to read indirect pointer block.\n");
        goto fail;
    }

    fprintf(stderr,
            "SFS: basic_write: "
            "i = %d, indirect_ptrs[i-12] = 0x%04x, remaining = %u\n",
            i,
            indirect_ptrs[i - SFS_DIRECT_PTR_COUNT],
            remaining);

    for(;
            i < end_block_n &&
            i < block_ptrs_per_block &&
            indirect_ptrs[i - SFS_DIRECT_PTR_COUNT] != SFS_NULL &&
            remaining > 0;

            i++,
            dst_offset = 0,
            src_offset += copy_count,
            remaining -= copy_count)
    {
        const unsigned int indirect_i = i - SFS_DIRECT_PTR_COUNT;

        fprintf(stderr,
                "SFS: basic_write: %u remaining bytes to write (i = %u).\n",
                remaining,
                i);

        // allocate a block-sized buffer
        memset(block_buf, 0, sb->block_size);

        // read any existing disk data into the block buffer
        if(read_blocks(
                    db_offset + indirect_ptrs[indirect_i],
                    1,
                    block_buf) < 1)
        {
            fprintf(stderr,
                    "SFS: basic_write: failed to read existing data from "
                    "indirect block %d (0x%04x).\n",
                    indirect_i,
                    indirect_ptrs[indirect_i]);
            goto fail;
        }

        // the number of bytes to copy is the size of a block minus the number
        // of bytes to skip at the beginning (this only matters for the first
        // block to write)
        copy_count = sb->block_size - dst_offset;

        // if that number of bytes is less than the number of remaining bytes
        // to write, then the number of bytes to copy is simply the number of
        // remaining bytes.
        if(remaining < copy_count)
            copy_count = remaining;

        // copy from the source buffer into the block buffer, overwriting disk
        // data where expected
        memcpy(block_buf + dst_offset, src + src_offset, copy_count);

        fprintf(stderr,
                "SFS: basic_write: copied %u bytes from source buffer "
                "(offset %u) to destination buffer (offset %u).\n",
                copy_count,
                src_offset,
                dst_offset);

        // write the block back to disk
        if(write_blocks(
                    db_offset + indirect_ptrs[indirect_i],
                    1,
                    block_buf) < 1)
        {
            fprintf(stderr,
                    "SFS: basic_write: failed to persist buffer for indirect "
                    "block %d (0x%04x).\n",
                    indirect_i,
                    indirect_ptrs[indirect_i]);
            goto fail;
        }

        fprintf(stderr,
                "SFS: basic_write: persisted indirect block %d (0x%04x).\n",
                indirect_i,
                indirect_ptrs[indirect_i]);
    }

    fprintf(stderr, "SFS: basic_write: successfully wrote %zu bytes.\n", size);

    free(block_buf);
    return (signed int)size;

fail:
    free(block_buf);
    return -1;
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

    const unsigned int end_block_n_maybe =
        (src_offset + size)
        / sb->block_size;

    // the number of blocks that the read spans
    const unsigned int end_block_n = end_block_n_maybe +
        ((src_offset + size) % sb->block_size == 0 ? 0 : 1);

    // adjust the source offset to be within the first block.
    src_offset -= start_block_n * sb->block_size;

    fprintf(stderr,
            "SFS: basic_read: reading %zu bytes, from data block %u up to "
            "%u, with first offset %u.\n",
            size,
            start_block_n,
            end_block_n,
            src_offset);

    // represents whether indirect blocks will be needed
    const char need_indirect = end_block_n > SFS_DIRECT_PTR_COUNT;

    // allocate a block-sized buffer to hold blocks read from disk.
    void *block_buf = calloc(sb->block_size, sizeof(char));

    // the block pointer index and the number of bytes to copy from the current
    // block (recalculated for each new block read)
    unsigned int i = start_block_n, copy_count;

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
        fprintf(stderr,
                "SFS: basic_read: %u remaining bytes to read (i = %u).\n",
                remaining,
                i);

        // read the next block into the block buffer
        if(read_blocks(db_offset + inode->direct_blocks[i], 1, block_buf) < 1)
        {
            fprintf(stderr,
                    "SFS: basic_read: failed to read direct block %d.\n",
                    i);
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

        fprintf(stderr,
                "SFS: basic_read: copied %u bytes from direct block %d "
                "(block 0x%04x) (offset %u) to destination (offset %u).\n",
                copy_count,
                i,
                inode->direct_blocks[i],
                src_offset,
                dst_offset);
    }

    // if no indirect blocks are needed, then we can simply return immediately.
    if(!need_indirect)
    {
        free(block_buf);
        fprintf(stderr,
                "SFS: basic_read: indirect block is unused, so exiting "
                "early.\n");
        return (signed int)size;
    }

    // otherwise, we read the indirect pointer block

    const unsigned int indirect_ptr_count =
        sb->block_size / sizeof(sfs_block_ptr);

    sfs_block_ptr indirect_ptrs[indirect_ptr_count];

    if(read_blocks(
                db_offset + inode->indirect_block,
                1,
                (void*)indirect_ptrs) < 0)
    {
        free(block_buf);
        fprintf(stderr,
                "SFS: basic_read: failed to read indirect pointer block.\n");
        return -1;
    }

    for(;
            i < end_block_n &&
            i < indirect_ptr_count &&
            indirect_ptrs[i] != SFS_NULL &&
            remaining > 0;

            i++,
            dst_offset += copy_count,
            src_offset = 0,
            remaining -= copy_count)
    {
        const unsigned int indirect_i = i - SFS_DIRECT_PTR_COUNT;

        fprintf(stderr,
                "SFS: basic_read: %u bytes remain to be read (i = %u).\n",
                remaining,
                i);

        // read the next block into the block buffer
        if(read_blocks(
                    db_offset + indirect_ptrs[indirect_i],
                    1,
                    block_buf) < 1)
        {
            free(block_buf);
            fprintf(stderr,
                    "SFS: basic_read: failed to read indirect block %u.\n",
                    indirect_i);
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

        fprintf(stderr,
                "SFS: basic_read: copied %u bytes from indirect block %d "
                "(block 0x%04x) (offset %u) to destination (offset %u).\n",
                copy_count,
                indirect_ptrs[indirect_i],
                indirect_i,
                src_offset,
                dst_offset);
    }

    fprintf(stderr,
            "SFS: basic_read: %zu bytes read successfully.\n",
            size);

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
    if(file->file_offset + size > file->inode.size)
    {
        size = file->inode.size - file->file_offset;
        fprintf(stderr,
                "SFS: file_read: adjusted read size to %zu.\n",
                size);
    }

    // perform the read
    const int ret = basic_read(&file->inode, dst, size, file->file_offset);

    // advance the file offset if successful
    if(ret > 0)
    {
        fprintf(stderr,
                "SFS: file_read: successfully read %d bytes.\n",
                ret);
        file->file_offset += ret;
    }
    else
        fprintf(stderr,
                "SFS: file_read: fail.\n");

    // return the number of bytes read.
    return ret;
}

int
file_write(struct sfs_file *file, const void *buf, const size_t size)
{
    size_t remaining = size;
    size_t copy_count;

    while(remaining > 0)
    {
        fprintf(stderr,
                "SFS: file_write: (inode %u) %zu remaining bytes to write.\n",
                file->inode.n,
                remaining);

        if(file->buf_offset == file->buf_size && file_flush(file) < 0)
        {
            fprintf(stderr,
                    "SFS: file_write: (inode %u) failed to flush write "
                    "buffer.\n",
                    file->inode.n);
            return -2;
        }

        copy_count = file->buf_size - file->buf_offset;
        if(remaining < copy_count)
        {
            fprintf(stderr,
                    "SFS: file_write: (inode %u) copying all remaining "
                    "bytes.\n",
                    file->inode.n);
            copy_count = remaining;
        }
        else
            fprintf(stderr,
                    "SFS: file_write: (inode %u) copying %zu bytes to "
                    "buffer.\n",
                    file->inode.n,
                    copy_count);

        memcpy(file->buf + file->buf_offset,
                buf + size - remaining,
                copy_count);

        file->buf_offset += copy_count;
        remaining -= copy_count;
    }

    fprintf(stderr,
            "SFS: file_write: (inode %u) wrote %zu bytes successfully.\n",
            file->inode.n,
            size);
    return size;
}

int
file_seek(struct sfs_file *file, int offset, sfs_seek_origin origin)
{
    // flush outstanding write buffers
    if(file_flush(file) < 0)
    {
        fprintf(stderr,
                "SFS: file_seek: flushing file (inode %u) failed.\n",
                file->inode.n);
        return -2;
    }

    unsigned int new_offset = 0;

    if(origin == SFS_START)
        new_offset = offset;
    else if(origin == SFS_END)
        new_offset = file->inode.size + offset;
    else if(origin == SFS_HERE)
        new_offset = file->file_offset + offset;
    else
    {
        fprintf(stderr,
                "SFS: file_seek: invalid seek origin.\n");
        return -3;
    }

    if(new_offset == file->file_offset)
    {
        fprintf(stderr,
                "SFS: file_seek: skipping pointless seek for inode %u.\n",
                file->inode.n);
        return (signed int)new_offset;
    }

    // check that desired position is not beyond the end of the file.
    if(new_offset > file->inode.size)
    {
        fprintf(stderr,
                "SFS: file_seek: seek inode %u -> %d is beyond end of file "
                "%zu.\n",
                file->inode.n,
                new_offset,
                file->inode.size);
        return -1;
    }

    fprintf(stderr,
            "SFS: file_seek: seek successful of inode %u: %u -> %d.\n",
            file->inode.n,
            file->file_offset,
            new_offset);

    // set the offset
    file->file_offset = new_offset;

    return (signed int)file->file_offset;
}

int
file_flush(struct sfs_file *file)
{
    // if there's nothing in the write buffer, then there's nothing to do.
    if(file->buf_offset == 0)
        return 0;

    // if the new file offset position is outside the file, then we need to
    // grow the file.
    const size_t new_file_offset = file->file_offset + file->buf_offset;
    if(new_file_offset > file->inode.size)
    {
        fprintf(stderr,
                "SFS: file_flush: growing file (inode %u) %zu -> %zu\n",
                file->inode.n,
                file->inode.size,
                new_file_offset);

        if(file_truncate(file, new_file_offset) != 0)
        {
            fprintf(stderr,
                    "SFS: file_flush: failed to truncate file to new size.\n");
            return -1;
        }

        // sanity check
        if(file->inode.size != new_file_offset)
        {
            fprintf(stderr,
                    "SFS: file_flush: new file size was not set.\n");
            return -1;
        }
    }

    // the call to file_truncate will guarantee the existence of an indirect
    // block if one happens to now be needed.

    // write the buffer to the file
    int ret = basic_write(
            &file->inode,
            file->buf,
            file->buf_offset,
            file->file_offset);

    if(ret < 0)
    {
        fprintf(stderr,
                "SFS: file_flush: failed to write buffer for inode %u.\n",
                file->inode.n);
        return -1;
    }

    fprintf(stderr,
            "SFS: file_flush: flushed %d bytes to disk.\n",
            ret);

    // adjust the offset pointers
    file->file_offset += file->buf_offset;
    file->buf_offset = 0;

    return ret;
}

int
file_close(struct sfs_file *file)
{
    if(file_flush(file) < 0)
    {
        fprintf(stderr,
                "SFS: file_close: failed to flush file (inode %u).\n",
                file->inode.n);
        return -2;
    }

    free(file->buf);

    fprintf(stderr,
            "SFS: file_close: inode %u, size %zu\n",
            file->inode.n,
            file->inode.size);
    free(file);
    return 0;
}

int resize_direct(
        sfs_block_ptr *buf,
        size_t used_count,
        size_t needed_count)
{
    const char is_alloc = needed_count > used_count;
    unsigned int i;

    fprintf(stderr,
            "SFS: resize_direct: %zu -> %zu blocks.\n",
            used_count,
            needed_count);

    if(is_alloc)
    {
        sfs_block_ptr *new_blocks = NULL;

        const unsigned int block_diff = needed_count - used_count;

        fprintf(stderr,
                "SFS: resize_direct: allocating %d new direct pointers\n",
                block_diff);

        // if we're unable to allocate the needed blocks, then fail.
        if((new_blocks = balloc(block_diff)) == NULL) goto fail;

        // write the pointers to the allocated blocks into the indirect block
        // buffer
        for(i = used_count; i < needed_count; i++)
            buf[i] = new_blocks[i - used_count];

        // we don't need the array of block pointers anymore.
        free(new_blocks);
    }
    else
    {
        const unsigned int block_diff =
            used_count - needed_count;

        if(block_diff == 0)
        {
            fprintf(stderr,
                    "SFS: resize_direct: pointless resize skipped.\n");
            goto done;
        }

        fprintf(stderr,
                "SFS: resize_direct: deleting %d direct pointers\n",
                block_diff);

        // mark them as free
        if(bfree(buf + needed_count * sizeof(*buf),
                used_count - needed_count) == -1)
            goto fail;

        // zero them out in the buffer
        for(i = needed_count; i < used_count; i++)
            buf[i] = SFS_NULL;
    }

done:
    return 0;

fail:
    return -1;
}

int
resize_indirect(
        const struct sfs_superblock *sb,
        struct sfs_inode *inode,
        size_t used_indirect_count,
        size_t needed_indirect_count)
{
    fprintf(stderr,
            "SFS: resize_indirect: %zu -> %zu blocks.\n",
            used_indirect_count,
            needed_indirect_count);

    if(used_indirect_count == needed_indirect_count)
    {
        fprintf(stderr, "SFS: resize_indirect: skipping pointless resize.\n");
        return 0;
    }

    sfs_block_ptr *buf = NULL;
    sfs_block_ptr *indirect_ptr = NULL;

    buf = calloc(1, sb->block_size * sizeof(char));

    // if we already have an indirect block, read it into memory.
    if(used_indirect_count != 0)
    {
        if(read_blocks(
                    get_data_blocks_offset(sb) + inode->indirect_block,
                    1,
                    buf) == -1)
        {
            fprintf(stderr,
                    "SFS: resize_indirect: failed to read indirect block.\n");
            goto fail;
        }
    }
    // if it turns out that we don't already have an indirect block on disk
    else
    {
        // allocate a pointer for it, and fail if we can't.
        indirect_ptr = balloc(1);
        if(indirect_ptr == NULL)
        {
            fprintf(stderr,
                    "SFS: resize_indirect: failed to allocate indirect "
                    "block.\n");
            goto fail;
        }

        // write the indirect pointer into the inode.
        inode->indirect_block = indirect_ptr[0];
        free(indirect_ptr);
    }

    if(resize_direct(buf, used_indirect_count, needed_indirect_count) < 0)
    {
        fprintf(stderr,
                "SFS resize_indirect: failed to resize.\n");
        goto fail;
    }

    // write the buffer to disk
    if(write_blocks(
            get_data_blocks_offset(sb) + inode->indirect_block,
            1,
            buf) < 0)
    {
        fprintf(stderr,
                "SFS: resize_indirect: failed to persist indirect block.\n");
        goto fail;
    }

    // if we don't need an indirect block anymore
    if(needed_indirect_count == 0)
    {
        // mark it as free
        bfree(&inode->indirect_block, 1);
        // remove the inode's pointer to it.
        inode->indirect_block = SFS_NULL;
    }

    free(buf);
    return 0;

fail:
    free(buf);
    return -1;
}

int
file_truncate(struct sfs_file *file, size_t new_size)
{
    const struct sfs_superblock *sb = load_superblock();

    const size_t max_size = get_max_file_size(sb);
    if(new_size > max_size)
    {
        fprintf(stderr,
                "SFS: file_truncate: "
                "file (inode %u) would grow to %zu > max file size %zu!\n",
                file->inode.n,
                new_size,
                max_size);
        return -1;
    }

    struct sfs_inode *inode = &file->inode;

    const size_t allocated_blocks = block_ceiling(sb, inode->size);
    const size_t needed_blocks = block_ceiling(sb, new_size);

    const size_t allocated_direct_blocks =
        allocated_blocks < SFS_DIRECT_PTR_COUNT ?
        allocated_blocks : SFS_DIRECT_PTR_COUNT;
    const size_t needed_direct_blocks =
        needed_blocks < SFS_DIRECT_PTR_COUNT ?
        needed_blocks : SFS_DIRECT_PTR_COUNT;

    const size_t allocated_indirect_blocks =
        allocated_blocks - allocated_direct_blocks;
    const size_t needed_indirect_blocks =
        needed_blocks - needed_direct_blocks;

    if(resize_indirect(
            sb,
            inode,
            allocated_indirect_blocks,
            needed_indirect_blocks) != 0)
    {
        fprintf(stderr,
                "SFS: file_truncate: indirect resize failed (inode %u).\n",
                file->inode.n);
        return -1;
    }

    if(resize_direct(
            inode->direct_blocks,
            allocated_direct_blocks,
            needed_direct_blocks) != 0)
    {
        fprintf(stderr,
                "SFS: file_truncate: direct resize failed (inode %u).\n",
                file->inode.n);
        return -1;
    }

    inode->size = new_size;
    fprintf(stderr,
            "SFS: file_truncate: inode %u size -> %zu\n",
            inode->n,
            new_size);
    if(file->file_offset > inode->size)
        file->file_offset = inode->size;

    inode_persist(sb, inode);

    return 0;
}

int
file_eof(struct sfs_file *file)
{
    return file->file_offset == file->inode.size;
}

struct free_bitfield *
load_bitfield(
        size_t block_size,
        size_t block_offset,
        size_t block_count,
        size_t item_count)
{
    fprintf(stderr,
            "SFS: load_bitfield: loading at block %zu (%zu blocks long).\n",
            block_offset,
            block_count);
    void *buf = calloc(block_count, block_size);
    read_blocks(block_offset, block_count, buf);
    struct free_bitfield *field = malloc(sizeof(*field));
    field->bit_count = item_count;
    field->bits = buf;
    return field;
}

void
free_bitfield(struct free_bitfield *bitfield)
{
    free(bitfield->bits);
    free(bitfield);
}

struct free_bitfield *
load_free_blocks_bitfield()
{
    const struct sfs_superblock *sb = load_superblock();
    return load_bitfield(
            sb->block_size,
            get_block_bitmap_offset(sb),
            sb->block_bitmap_count,
            sb->block_count);
}

struct free_bitfield *
load_free_inodes_bitfield()
{
    const struct sfs_superblock *sb = load_superblock();
    return load_bitfield(
            sb->block_size,
            get_inode_bitmap_offset(sb),
            sb->inode_bitmap_count,
            sb->inode_count);
}

int
bitfield_persist(
        const struct sfs_superblock *sb,
        const struct free_bitfield *field,
        size_t block_offset)
{
    const size_t block_size = sb->block_size;
    size_t block_count =
        field->bit_count / CHAR_BIT / block_size;
    if((field->bit_count / CHAR_BIT) % block_size != 0)
        block_count++;
    int ret = write_blocks(block_offset, block_count, field->bits);
    if(ret >= 0)
        fprintf(stderr,
                "SFS: bitfield_persist: persisted bitfield at block %zu.\n",
                block_offset);
    else
        fprintf(stderr,
                "SFS: bitfield_persist: failed to persist bitfield at block "
                "%zu.\n",
                block_offset);
    return ret;
}

int
inode_persist(
        const struct sfs_superblock *sb,
        const struct sfs_inode *inode)
{
    const size_t block_size = sb->block_size;
    const size_t inodes_per_block = get_inodes_per_block(sb);
    const unsigned int inode_block = inode->n / inodes_per_block;

    struct sfs_inode *buf = calloc(block_size, sizeof(char));
    if(read_blocks(get_inode_table_offset(sb) + inode_block, 1, buf) < 0)
        goto fail;

    const size_t skipped_inodes = inode_block * inodes_per_block;
    const unsigned int inode_offset = inode->n - skipped_inodes;
    buf[inode_offset] = *inode;
    if(write_blocks(get_inode_table_offset(sb) + inode_block, 1, buf) < 0)
        goto fail;

    fprintf(stderr,
            "SFS: inode_persist: wrote inode %u to disk.\n",
            inode->n);
    return 0;

fail:
    free(buf);
    return -1;
}

int
bitpack_next_free(bitpack pack, bitpack_scan position)
{
    for(; position < BITPACK_SCAN_END; position++)
        if((pack & (1U << position)) == 0)
            return position;
    return -1;
}

unsigned short *
find_free_items(size_t count, struct free_bitfield *field)
{
    fprintf(stderr,
            "SFS: find_free_items: searching %zu bits for %zu free items.\n",
            field->bit_count,
            count);
    // Sanity check that the number of bits in the bitfield is divisible by
    // the width of a bitpack.
    if(field->bit_count % BITPACK_SIZE != 0)
    {
        fprintf(stderr,
                "EDGE CASE: number of bits must be divisible by number of "
                "bits in a bitpack.\n");
    }

    // array of items we found so far
    unsigned short *found_items = calloc(count, sizeof(*found_items));

    // index in that array to store the next found item
    unsigned int next_index = 0;

    // Traverse the bitfield until we've found the desired number of items or
    // we hit the end of the bitfield
    unsigned int i;
    for(i = 0;
            i * BITPACK_SIZE < field->bit_count &&
            next_index < count;

            i++)
    {
        // load the bitpack from the bitfield.
        const bitpack pack = field->bits[i];

        fprintf(stderr,
                "SFS: find_free_items: scanning bitpack #%u, 0x%08x.\n",
                i,
                pack);

        // find the next free index in the bitpack
        int next_free = BITPACK_SCAN_START;
        while((next_free = bitpack_next_free(pack, next_free)) != -1
                && next_index < count)
        {
            // for each one we find, add it to the array of found items.
            found_items[next_index++] =
                i * BITPACK_SIZE + (unsigned short)next_free;
            fprintf(stderr,
                    "SFS: find_free_items: found free item at position %u.\n",
                    found_items[next_index - 1]);
            next_free++;
        }
    }

    // if we hit the end of the bitfield without finding enough free items,
    // then we're shit outta luck !
    if(next_index != count)
    {
        fprintf(stderr,
                "SFS: find_free_items: fail: found %u < %zu free items.\n",
                next_index,
                count);
        free(found_items);
        return NULL;
    }
    else
    {
        return found_items;
    }
}

void
bitfield_mark(
        struct free_bitfield *field,
        unsigned short *ptrs,
        size_t count,
        bitfield_value t)
{
    unsigned int i;
    for(i = 0; i < count; i++)
    {
        // fetch the pointer we're interested in
        const unsigned short p = ptrs[i];

        // calculate which bitpack that pointer refers to
        const unsigned int bitpack_offset = p / BITPACK_SIZE;

        // compute the offset within that bitpack
        const unsigned int bit_offset =
            p - bitpack_offset * BITPACK_SIZE;

        // fetch the bitpack we're interested in
        bitpack pack = field->bits[bitpack_offset];

        fprintf(stderr,
                "SFS: bitfield_mark: mark bit %u (offset %u in pack %u) as "
                "%u.\n",
                p,
                bit_offset,
                bitpack_offset,
                t);

        // set the bit_offset-th bit in pack if t is 1; else, clear it.
        pack ^= (-t ^ pack) & (1U << bit_offset);

        // write the bitpack back to the bitfield.
        field->bits[bitpack_offset] = pack;

        fprintf(stderr,
                "SFS: bitfield_mark: pack after: 0x%08x.\n",
                field->bits[bitpack_offset]);

    }
}

sfs_block_ptr *
balloc(size_t count)
{
    struct sfs_superblock *sb = load_superblock();

    fprintf(stderr,
            "SFS: balloc: allocating %zu new blocks.\n",
            count);

    struct free_bitfield *block_bitfield = load_free_blocks_bitfield();
    sfs_block_ptr *free_blocks =
        (sfs_block_ptr *)find_free_items(count, block_bitfield);

    // abort if we can't find enough free blocks
    if(free_blocks == NULL)
        return NULL;

    // mark the blocks as used
    bitfield_mark(block_bitfield, free_blocks, count, BIT_USED);

    // persist the bitfield to disk
    if(bitfield_persist(sb, block_bitfield, get_block_bitmap_offset(sb)) == -1)
    {
        // if the persistence fails, free the list of found free items
        free(free_blocks);
        return NULL;
    }

    return free_blocks;
}

int
bfree(sfs_block_ptr *blocks, size_t count)
{
    const struct sfs_superblock *sb = load_superblock();
    struct free_bitfield *block_bitfield = load_free_blocks_bitfield();

    // mark the blocks as free.
    bitfield_mark(block_bitfield, blocks, count, BIT_FREE);

    // persist the bitfield to disk.
    return bitfield_persist(sb, block_bitfield, get_block_bitmap_offset(sb));
}

sfs_inode_n
ialloc()
{
    struct sfs_superblock *sb = load_superblock();

    fprintf(stderr,
            "SFS: ialloc: allocating a new inode.\n");

    struct free_bitfield *inode_bitfield = load_free_inodes_bitfield();
    sfs_inode_n *free_inodes =
        (sfs_inode_n *)find_free_items(1, inode_bitfield);

    // abort if we have no more inodes
    if(free_inodes == NULL)
        return SFS_INODE_NULL;

    bitfield_mark(inode_bitfield, (unsigned short*)free_inodes, 1, BIT_USED);

    if(bitfield_persist(sb, inode_bitfield, get_inode_bitmap_offset(sb)) == -1)
    {
        free(free_inodes);
        return SFS_INODE_NULL;
    }

    return free_inodes[0];
}

int
ifree(sfs_inode_n *inodes, size_t count)
{
    const struct sfs_superblock *sb = load_superblock();
    struct free_bitfield *inode_bitfield = load_free_inodes_bitfield();

    // mark the inodes as free.
    bitfield_mark(inode_bitfield, inodes, count, BIT_FREE);

    // persist the bitfield to disk.
    return bitfield_persist(sb, inode_bitfield, get_inode_bitmap_offset(sb));
}

struct sfs_inode *
new_inode(sfs_inode_n n, sfs_mode mode)
{
    struct sfs_inode *inode = malloc(sizeof(*inode));
    *inode = (struct sfs_inode) {
        .n = n,
        .mode = mode,
        .link_count = 1,
        .uid = 0,
        .gid = 0,
        .size = 0,
        .indirect_block = SFS_NULL
    };

    int i;
    for(i = 0; i < SFS_DIRECT_PTR_COUNT; i++)
        inode->direct_blocks[i] = SFS_NULL;

    fprintf(stderr, "SFS: new_inode: allocated inode %d in memory.\n", n);

    return inode;
}

char *
dump_inode(struct sfs_inode *inode)
{
    const struct sfs_superblock *sb = load_superblock();

    int result1, result3;

    char *s1 = NULL, *s2 = NULL, *s3 = NULL, *s4 = NULL;
    result1 = asprintf(
            &s1,
            "inode %u\n"
            "size: %zu\n"
            "mode: %u\n"
            "links: %u\n"
            "uid: %u\n"
            "gid: %u\n"
            "direct pointers:\n",
            inode->n,
            inode->size,
            inode->mode,
            inode->link_count,
            inode->uid,
            inode->gid);

    if(result1 < 0)
        return NULL;

    const size_t line_size = 11;

    s2 = calloc(SFS_DIRECT_PTR_COUNT, line_size * sizeof(char) + 1);

    int i;
    for(i = 0; i < SFS_DIRECT_PTR_COUNT; i++)
        sprintf(s2 + line_size * i, "0x%04x\n", inode->direct_blocks[i]);

    sfs_block_ptr *indirect_blocks = NULL;

    if(inode->indirect_block == SFS_NULL)
    {
        s3 = strdup("No indirect blocks.");
        s4 = strdup("\0");
    }
    else
    {
        indirect_blocks = calloc(1, sb->block_size * sizeof(char));
        read_blocks(
                get_data_blocks_offset(sb) + inode->indirect_block,
                1,
                indirect_blocks);

        result3 = asprintf(
                &s3,
                "indirect pointer: 0x%04x; blocks:\n",
                inode->indirect_block);

        if(result3 < 0)
        {
            free(s1);
            free(s2);
            return NULL;
        }

        const size_t indirect_block_count =
            (inode->size - sb->block_size * SFS_DIRECT_PTR_COUNT)
            / sb->block_size;

        s4 = calloc(indirect_block_count, line_size * sizeof(char) + 1);

        for(i = 0; i < indirect_block_count; i++)
            sprintf(
                    s4 + i * line_size,
                    "0x%04x\n",
                    indirect_blocks[i]);
    }

    char * result = calloc(
            1,
            1 + sizeof(char) * (
                strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4)));

    strcat(result, s1);
    strcat(result, s2);
    strcat(result, s3);
    strcat(result, s4);
    free(s1);
    free(s2);
    free(s3);
    free(s4);

    return result;
}
