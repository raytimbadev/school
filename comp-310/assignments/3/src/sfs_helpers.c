#include "sfs_helpers.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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
    unsigned int bits_per_block = block_size * CHAR_BIT;

    unsigned int inode_blocks = inode_count / inodes_per_block + 1;

    unsigned int data_bitmap_blocks = block_count / bits_per_block + 1;
    unsigned int inode_bitmap_blocks = inode_count / bits_per_block + 1;

    unsigned int total_blocks = 1 +
        block_count + inode_blocks +
        data_bitmap_blocks + inode_bitmap_blocks;

    // TODO check for errors
    if(fresh == SFS_FRESH)
    {
        if(init_fresh_disk(path, block_size, total_blocks) != 0)
            return -1;
    }
    else
    {
        if(init_disk(path, block_size, total_blocks) != 0)
            return -1;
    }

    SUPERBLOCK = malloc(sizeof(*SUPERBLOCK));
    SUPERBLOCK->magic = MAGIC;
    SUPERBLOCK->block_size = block_size;
    SUPERBLOCK->block_count = block_count;
    SUPERBLOCK->inode_count = inode_count;
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
    size_t blocks = bytes / superblock->block_size;
    if(bytes % superblock->block_size != 0)
        blocks++;
    return blocks;
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
        struct sfs_inode *parent,
        const sfs_mode mode)
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

    sfs_inode_n child_inode = dirlookup(dir, path);
    free_dir_iter(dir);

    // if the child doesn't exist
    if(child_inode == SFS_INODE_NULL)
    {
        // and if we've reached the end of the path and we have a mode, then we
        // need to create a new file.
        if(done && mode != SFS_NO_MODE)
            goto create;
        // otherwise, if we're not done or if we don't have a mode, then this
        // is a file not found error.
        else
            return -4;
    }

    if(load_inode(child_inode, &parent) != 0)
        return -5;

    if(!done)
        goto recurse;

    // if we're done then the parent pointer in fact points to the file
    // identified by the full path. We can simply set *inode to parent then
    // to return this inode struct to the top caller.

    *inode = parent;

    return 0;

create:
    // allocate a new inode number
    if((child_inode = ialloc()) == SFS_INODE_NULL)
        return -6;

    // overwrite the inode with our new data
    (*inode)->n = child_inode;
    (*inode)->mode = mode;
    (*inode)->link_count = 1;
    (*inode)->uid = 0;
    (*inode)->gid = 0;
    (*inode)->size = 0;
    int i;
    for(i = 0; i < SFS_DIRECT_PTR_COUNT; i++)
        (*inode)->direct_blocks[i] = SFS_NULL;
    (*inode)->indirect_block = SFS_NULL;

    // write the inode to disk.
    inode_persist(load_superblock(), *inode);

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

    // check that the file is a directory
    if((inode->mode & MODE_D) == 0)
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
file_open(
        const struct sfs_inode *inode,
        struct sfs_file **file)
{
    if(*file == NULL)
        *file = malloc(sizeof(**file));

    **file = (struct sfs_file) {
        .inode = *inode,
        .file_offset = 0,
        .buf_offset = 0,
        .buf = calloc(SFS_WRITEBUF_SIZE, sizeof(char)),
        .buf_size = SFS_WRITEBUF_SIZE
    };

    return 0;
}

int link_remove(
        const struct sfs_inode *dir_inode,
        const char *filename)
{
    struct sfs_file *file = NULL;

    // open the directory
    if(file_open(dir_inode, &file) != 0)
        return -1;

    // marker for the position in the directory where we found a match
    unsigned int found_at = 0;
    char found = 0;
    struct sfs_dir_entry entry;
    int read_count;

    // scan the directory, looking for files with the name we're looking for
    while(!file_eof(file))
    {
        read_count = file_read(file, &entry, sizeof(entry));

        if(read_count < sizeof(entry))
            break;

        if(strncmp(filename, entry.filename, MAXFILENAME) != 0)
            continue;

        found = 1;
        found_at = file_seek(file, 0, SFS_HERE) - sizeof(entry);

        break;
    }

    // no link to remove
    if(!found)
        return 0;

    size_t remaining = file->inode.size - found_at + sizeof(entry);

    const size_t buf_size = remaining + sizeof(sfs_inode_n);
    void *buf = calloc(buf_size, sizeof(char));

    read_count = file_read(file, buf, buf_size);
    file_seek(file, found_at, SFS_START);
    file_write(file, buf, buf_size);

    // we've now eliminated the old file entry by moving the subsequent entries
    // forward.
    file_close(file);

    // now we need to unlink the old inode
    struct sfs_inode *old_inode = NULL;

    // load the old inode and decrement its link count. If it still has files
    // pointing to it, then we can persist the modified inode and return 1 (the
    // number of files that we've unlinked)
    load_inode(entry.inode, &old_inode);
    old_inode->link_count--;
    if(old_inode->link_count > 0)
    {
        inode_persist(load_superblock(), old_inode);
        free(old_inode);
        return 1;
    }

    // otherwise, the link_count is zero! Meaning that we need to properly
    // delete the file.

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

    // return the number of unlinked files.
    return 1;
}

int
link_create(
        const struct sfs_inode *dir_inode,
        const struct sfs_dir_entry *entry)
{
    struct sfs_file *file = NULL;

    // remove any existing link to the file we want to create here
    link_remove(dir_inode, entry->filename);

    // open the directory
    if(file_open(dir_inode, &file) != 0)
        return -1;

    // write our new directory entry to the end of the file.
    if(file_write(file, entry, sizeof(entry) < 0))
        goto fail;

    // close the file.
    if(file_close(file) < 0)
        goto fail;

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

    // the number of blocks that the read spans
    const unsigned int needed_blocks_maybe = size / sb->block_size;
    const unsigned int needed_blocks = needed_blocks_maybe +
        (size % sb->block_size == 0 ? 0 : 1);

    const unsigned int end_block_n = start_block_n + needed_blocks;

    // adjust the destination offset to fall within a block.
    dst_offset -= start_block_n * sb->block_size;

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

            i ++,
            dst_offset = 0,
            src_offset += copy_count,
            remaining -= copy_count)
    {
        // allocate a block-sized buffer
        memset(block_buf, 0, sb->block_size);

        // read any existing disk data into the block buffer
        if(read_blocks(
                    db_offset + inode->direct_blocks[i],
                    1,
                    block_buf) < 1)
            goto fail;

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

        // write the block back to disk
        if(write_blocks(
                    db_offset + inode->direct_blocks[i],
                    1,
                    block_buf) < 1)
            goto fail;
    }

    // if no indirect blocks are needed, then we can simply return immediately.
    if(!need_indirect)
    {
        free(block_buf);
        return (signed int)size;
    }

    if(read_blocks(
                db_offset + inode->indirect_block,
                1,
                (void*)indirect_ptrs) <0)
        goto fail;

    for(;
            i < end_block_n &&
            i < block_ptrs_per_block &&
            indirect_ptrs[i] != SFS_NULL &&
            remaining > 0;

            i++,
            dst_offset = 0,
            src_offset += copy_count,
            remaining -= copy_count)
    {
        // allocate a block-sized buffer
        memset(block_buf, 0, sb->block_size);

        // read any existing disk data into the block buffer
        if(read_blocks(
                    db_offset + indirect_ptrs[i],
                    1,
                    block_buf) < 1)
            goto fail;

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

        // write the block back to disk
        if(write_blocks(
                    db_offset + indirect_ptrs[i],
                    1,
                    block_buf) < 1)
            goto fail;
    }

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

    // the number of blocks that the read spans
    const unsigned int needed_blocks_maybe = size / sb->block_size;
    const unsigned int needed_blocks = needed_blocks_maybe +
        (size % sb->block_size == 0 ? 0 : 1);

    // comes out to one plus the last needed index
    const unsigned int end_block_n = start_block_n + needed_blocks;

    // adjust the source offset to be within the first block.
    src_offset -= start_block_n * sb->block_size;

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

    for(;
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
    if(file->file_offset + size > file->inode.size)
        size = file->inode.size - file->file_offset;

    // perform the read
    const int ret = basic_read(&file->inode, dst, size, file->file_offset);

    // advance the file offset if successful
    if(ret > 0)
        file->file_offset += ret;

    // return the number of bytes read.
    return ret;
}

int
file_write(struct sfs_file *file, const void *buf, size_t size)
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
file_seek(struct sfs_file *file, int offset, sfs_seek_origin origin)
{
    // flush outstanding write buffers
    if(file_flush(file) < 0)
        return -2;

    unsigned int new_offset = 0;

    if(origin == SFS_START)
        new_offset = offset;
    else if(origin == SFS_END)
        new_offset = file->inode.size + offset;
    else if(origin == SFS_HERE)
        new_offset = file->file_offset + offset;
    else
        return -3;

    // check that desired position is not beyond the end of the file.
    if(new_offset > file->inode.size)
        return -1;

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
        if(file_truncate(file, new_file_offset) != 0)
            return -1;

    // the call to file_truncate will guarantee the existence of an indirect
    // block if one happens to now be needed.

    // write the buffer to the file
    basic_write(&file->inode, file->buf, file->buf_offset, file->file_offset);

    // adjust the offset pointers
    file->file_offset += file->buf_offset;
    file->buf_offset = 0;

    return 0;
}

int
file_close(struct sfs_file *file)
{
    if(file_flush(file))
        return -2;

    free(file->buf);
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

    if(is_alloc)
    {
        sfs_block_ptr *new_blocks = NULL;

        const unsigned int block_diff = needed_count - used_count;

        fprintf(stderr,
                "allocating %d new indirect pointers\n", block_diff);

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

        fprintf(stderr, "deleting %d indirect pointers\n", block_diff);

        // mark them as free
        if(bfree(buf + needed_count * sizeof(*buf),
                used_count - needed_count) == -1)
            goto fail;

        // zero them out in the buffer
        for(i = needed_count; i < used_count; i++)
            buf[i] = SFS_NULL;
    }

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
    if(used_indirect_count == needed_indirect_count)
    {
        fprintf(stderr, "indirect block resize with no effect.\n");
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
            goto fail;
    }
    // if it turns out that we don't already have an indirect block on disk
    else
    {
        // allocate a pointer for it, and fail if we can't.
        indirect_ptr = balloc(1);
        if(indirect_ptr == NULL) goto fail;

        // write the indirect pointer into the inode.
        inode->indirect_block = indirect_ptr[0];
        free(indirect_ptr);
    }

    resize_direct(buf, used_indirect_count, needed_indirect_count);

    // write the buffer to disk
    write_blocks(
            get_data_blocks_offset(sb) + inode->indirect_block,
            1,
            buf);

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

    struct sfs_inode *inode = &file->inode;

    const size_t allocated_blocks = block_ceiling(sb, inode->size);
    const size_t needed_blocks = block_ceiling(sb, new_size);

    if(allocated_blocks == needed_blocks)
        return 0;

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
        return -1;

    if(resize_direct(
            inode->direct_blocks,
            allocated_direct_blocks,
            needed_direct_blocks) != 0)
        return -1;

    inode->size = new_size;
    if(file->file_offset > inode->size)
        file->file_offset = inode->size;

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
    void *buf = calloc(block_count, block_size);
    read_blocks(block_offset, block_count, buf);
    struct free_bitfield *field = malloc(sizeof(*field));
    field->bit_count = item_count;
    field->bits = buf;
    return field;
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
    return write_blocks(block_offset, block_count, field->bits);
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

    return 0;

fail:
    free(buf);
    return -1;
}

int
bitpack_scan_conv(bitpack_scan scan)
{
    // this is insanely inefficient.
    int i;
    for(i = -1; scan > 0; scan >>= 1, i++);
    return i;
}

int
bitpack_next_free(bitpack pack, bitpack_scan *position)
{
    if(position == BITPACK_SCAN_END)
        return 0;

    bitpack_scan here = *position;
    *position <<= 1;

    if((pack & here) != 0)
        return bitpack_scan_conv(here);
    else {
        return bitpack_next_free(pack, position);
    }
}

unsigned int *
find_free_items(size_t count, struct free_bitfield *field)
{
    // Sanity check that the number of bits in the bitfield is divisible by
    // the width of a bitpack.
    if(field->bit_count % sizeof(*field->bits) != 0)
    {
        fprintf(stderr,
                "EDGE CASE: number of bits must be divisible by number of "
                "bits in a bitpack.\n");
    }

    // array of items we found so far
    unsigned int *found_items = calloc(count, sizeof(*found_items));

    // index in that array to store the next found item
    unsigned int next_index = 0;

    // Traverse the bitfield until we've found the desired number of items or
    // we hit the end of the bitfield
    unsigned int i;
    for(i = 0;
            i < field->bit_count &&
            next_index < count;

            i += sizeof(*field->bits))
    {
        // load the bitpack from the bitfield.
        const bitpack pack = field->bits[i / sizeof(*field->bits)];
        bitpack_scan scan = BITPACK_SCAN_START;

        // find the next free index in the bitpack
        int next_free;
        while((next_free = bitpack_next_free(pack, &scan)) != -1
                && next_index < count)
        {
            // for each one we find, add it to the array of found items.
            found_items[next_index++] = i + (unsigned int)next_free;
        }
    }

    // if we hit the end of the bitfield without finding enough free items,
    // then we're sheer outta luck !
    if(next_index != count)
    {
        free(found_items);
        return NULL;
    }
    else
        return found_items;
}

void
bitfield_mark(
        struct free_bitfield *field,
        unsigned int *ptrs,
        size_t count,
        bitfield_value t)
{
    unsigned int i;
    for(i = 0; i < count; i++)
    {
        // fetch the pointer we're interested in
        const unsigned int p = ptrs[i];

        // calculate which bitpack that pointer refers to
        const unsigned int bitpack_offset = p / sizeof(*field->bits);

        // compute the offset within that bitpack
        const unsigned int bit_offset =
            p - bitpack_offset * sizeof(*field->bits);

        // fetch the bitpack we're interested in
        bitpack pack = field->bits[bitpack_offset];

        // set the bit_offset-th bit in pack if t is 1; else, clear it.
        pack ^= (-t ^ pack) & (1 << bit_offset);

        // write the bitpack back to the bitfield.
        field->bits[bitpack_offset] = pack;
    }
}

sfs_block_ptr *
balloc(size_t count)
{
    struct sfs_superblock *sb = load_superblock();

    struct free_bitfield *block_bitfield = load_free_blocks_bitfield();
    sfs_block_ptr *free_blocks =
        (sfs_block_ptr *)find_free_items(count, block_bitfield);

    // abort if we can't find enough free blocks
    if(free_blocks == NULL)
        return NULL;

    // mark the blocks as used
    bitfield_mark(block_bitfield, (unsigned int*)free_blocks, count, BIT_USED);

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

    struct free_bitfield *inode_bitfield = load_free_inodes_bitfield();
    sfs_inode_n *free_inodes =
        (sfs_inode_n *)find_free_items(1, inode_bitfield);

    // abort if we have no more inodes
    if(free_inodes == NULL)
        return NULL;

    bitfield_mark(inode_bitfield, (unsigned int*)free_inodes, 1, BIT_USED);

    if(bitfield_persist(sb, inode_bitfield, get_inode_bitmap_offset(sb)) == -1)
    {
        free(free_inodes);
        return NULL;
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
