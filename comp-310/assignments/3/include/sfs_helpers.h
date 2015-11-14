#ifndef SFS_HELPERS_H
#define SFS_HELPERS_H

#include "sfs_api.h"
#include "disk_emu.h"

/**
 * Initializes an SFS disk.
 */
int sfs_format(int fresh, char *path, size_t block_size, size_t block_count);

/**
 * Loads the superblock.
 */
struct sfs_superblock * load_superblock();

/**
 * Computes the number of blocks needed to store a given number of bytes.
 */
size_t block_ceiling(const struct sfs_superblock *superblock, size_t bytes);

/**
 * Gets the number of inodes that can fit on one block.
 */
size_t get_inodes_per_block(const struct sfs_superblock *sb);

/**
 * Gets the block offset of the beginning of the inode table.
 */
size_t get_inode_table_offset(const struct sfs_superblock *sb);

/**
 * Gets the block offset of the beginning of the data section.
 */
size_t get_data_blocks_offset(const struct sfs_superblock *sb);

/**
 * Gets the block offset of the free inode bitmap section.
 */
size_t get_inode_bitmap_offset(const struct sfs_superblock *sb);

/**
 * Gets the block offset of the free data block bitmap section.
 */
size_t get_block_bitmap_offset(const struct sfs_superblock *sb);

/**
 * Gets the maximum size of any file representable in SFS.
 */
size_t get_max_file_size(const struct sfs_superblock *sb);

/**
 * Allocates memory with block-size granularity.
 */
void * block_malloc(const struct sfs_superblock *superblock, size_t block_count);

/**
 * Determines what block a given inode is stored on.
 *
 * If `p` is not NULL, then the value pointed to by `p` is set to the index of
 * the first inode on the determined block.
 */
int resolve_inode_to_block(sfs_inode_n n, sfs_block_ptr *m, sfs_inode_n *p);

/**
 * Loads an inode from disk.
 *
 * If the value pointed to by `inode` is NULL, then an sfs_inode is allocated.
 * Else the provided sfs_inode is overwritten with the new data.
 *
 * In case of failure, no memory needs to be freed by the caller.
 */
int load_inode(sfs_inode_n n, struct sfs_inode **inode);

/**
 * Reads an entire file into memory at once. This function must be used with
 * caution.
 *
 * Even in case of failure, the caller must free the value pointed to by
 * `buf` if the original value pointed to `buf` is NULL.
 */
int read_whole_file(const struct sfs_inode *inode, void **buf);

/**
 * Follows a path, yielding the inode object of the identified file.
 *
 * `path` must be an absolute path.
 *
 * This function should always be called with `parent` as NULL.
 *
 * If the value pointed to by `inode` is NULL, then an sfs_inode will be
 * allocated. Else, the given sfs_inode will be overwritten.
 *
 * If the given file doesn't exist, then [if `mode` is SFS_NO_MODE, then an
 * error results, else a new file is created with that mode].
 */
int follow_path(
        char *path,
        struct sfs_inode **inode,
        struct sfs_inode *parent,
        const sfs_mode mode);

/**
 * Interprets a buffer as a directory listing.
 *
 * If the value pointed to by `iter` is NULL, then an sfs_dir_iter is
 * allocated. Else, the provided sfs_dir_iter is overwritten with the new data.
 *
 * Even in case of failure, the caller must free the value pointed to by
 * `iter`.
 */
int listdir(const struct sfs_inode *inode, struct sfs_dir_iter **iter);

/**
 * Looks up a given file name in a directory.
 *
 * Returns the inode number of the matched file. If no files match, returns
 * SFS_INODE_NULL.
 */
sfs_inode_n dirlookup(const struct sfs_dir_iter *dir, const char *filename);

/**
 * Free all memory associated with a directory iterator.
 */
void free_dir_iter(struct sfs_dir_iter *iter);

/**
 * Opens a file for read/write, seeks to the end of it, and allocates an
 * in-memory buffer for writes.
 *
 * The file object will get a copy of the given inode to use internally.
 */
int file_open(
        const struct sfs_inode *inode,
        struct sfs_file **file);

/**
 * Adds or overwrites a directory entry in a directory.
 */
int link_create(
        const struct sfs_inode *dir_inode,
        const struct sfs_dir_entry *entry);

/**
 * Removes a file from a directory, decrementing the link count of the file's
 * inode.
 *
 * If the link count of the file reaches zero, then the file is fully deleted,
 * i.e. its data blocks and its inode are released.
 *
 * If successful, returns the number of unlinked files, which is at most one.
 * Else, returns a negative number.
 */
int link_remove(
        const struct sfs_inode *dir_inode,
        const char *filename);

/**
 * Resize the write buffer of a file.
 *
 * If the new capacity is less than the current size, the buffer will be
 * flushed.
 *
 * The return value is -2 if the flush fails, -1 for other failures, and
 * nonnegative otherwise. A nonnegative return value represents the number of
 * bytes flushed.
 */
int file_buffer_resize(struct sfs_file *file, size_t new_size);

/**
 * Reads data from a file into a given buffer.
 *
 * If the write buffer is nonempty, it is flushed.
 *
 * Returns the number of bytes read, which is at most `size`. If an error
 * occurs, returns a negative value.
 */
int file_read(struct sfs_file *file, void *buf, size_t size);

/**
 * Writes data from a given buffer into the file's write buffer.
 *
 * If the write would overflow the buffer, then the buffer is flushed.
 * If the write would overflow an empty buffer, then -1 is returned.
 * TODO don't die on big writes!!
 *
 * Returns the number of bytes flushed to disk, which is at least zero.
 */
int file_write(struct sfs_file *file, const void *buf, size_t count);

/**
 * Seeks to a given offset from the start of the file.
 *
 * Returns the current file offset.
 */
int file_seek(struct sfs_file *file, int offset, sfs_seek_origin origin);

/**
 * Flushes the write buffer of a file to disk.
 *
 * If the flush would result in a file exceeding the maximum file size, then
 * it is not performed and -1 is returned.
 *
 * Else, the number of bytes flushed is returned.
 *
 * If the size of the file increases as a result of the flush, the inode of the
 * file is update and written to disk.
 */
int file_flush(struct sfs_file *file);

/**
 * Closes a file, flushing its buffers and freeing any memory associated with
 * it.
 */
int file_close(struct sfs_file *file);

/**
 * Directly resizes an indirect block buffer from a given used block count
 * to a given needed block count.
 *
 * Blocks are allocated or deallocated depending on the situation, and the
 * pointers to the allocated blocks or deallocated blocks are added or removed
 * from the indirect block buffer, respectively.
 */
int resize_direct(sfs_block_ptr *buf, size_t used_count, size_t needed_count);

/**
 * Resizes the indirect block of a file, automatically allocating or releasing
 * an indirect block for the file if one is now or no longer respectively
 * needed by it.
 *
 * Blocks will be allocated or freed for the file depending on the given values
 * of `used_indirect_count` which represents the currently used number of
 * indirect blocks and `needed_indirect_count` which is the target number of
 * blocks the file should now have.
 */
int resize_indirect_block(
        const struct sfs_superblock *sb,
        struct sfs_inode *inode,
        size_t used_indirect_count,
        size_t needed_indirect_count);

/**
 * Truncates a file, changing its effective size on disk, allocating / freeing
 * any blocks required to do so.
 *
 * If the file is truncated so that its current file pointer would go beyond
 * the end of the file, then the file pointer is adjusted to point to the end
 * of the file.
 */
int file_truncate(struct sfs_file *file, size_t new_size);

/**
 * Checks whether a file is at the end.
 */
int file_eof(struct sfs_file *file);

/**
 * Read a portion of a file into a given buffer.
 *
 * Automatically chases direct and indirect block pointers.
 */
int basic_read(
        const struct sfs_inode *inode,
        void *dst,
        size_t size,
        unsigned int src_offset);

struct free_bitfield *load_bitfield(
        size_t block_size,
        size_t block_offset,
        size_t block_count,
        size_t item_count);

/**
 * Fetches the bitfield describing the free data blocks in the filesystem.
 */
struct free_bitfield *load_free_blocks_bitfield();

/**
 * Fetches the bitfield describing the free inodes in the filesystem.
 */
struct free_bitfield *load_free_inodes_bitfield();

/**
 * Frees the memory associated with a bitfield.
 */
void free_bitfield(struct free_bitfield *bitfield);

/**
 * Writes a bitfield to disk at a given block offset.
 */
int bitfield_persist(
        const struct sfs_superblock *sb,
        const struct free_bitfield *field,
        size_t block_offset);

int inode_persist(
        const struct sfs_superblock *sb,
        const struct sfs_inode *inode);

/**
 * Scans a bitpack to find the next free item within it.
 *
 * Returns the index of the next free item within the bitpack. Returns -1 if
 * the scan is finished.
 *
 * This function is reentrant. It should initially be called with position
 * pointing to the value BITPACK_SCAN_START. When it ends, the value pointed
 * to by position will be BITPACK_SCAN_END.
 */
int bitpack_next_free(bitpack pack, bitpack_scan position);

/**
 * Finds the offsets of free items by lookup in a bitfield.
 *
 * Used to find free blocks or inodes by passing different bitfields.
 */
unsigned short *find_free_items(size_t count, struct free_bitfield *field);

/**
 * Marks each bit identified by successive pointers from a given list with
 * a specified value in a bitfield.
 */
void bitfield_mark(
        struct free_bitfield *field,
        unsigned short *ptrs,
        size_t count,
        bitfield_value t);

/**
 * Allocates a given number of blocks in the filesystem.
 *
 * Upon successful return, these blocks are marked in use and can no longer be
 * allocated. The return value is an array of block pointers, of which there
 * are exactly `count`.
 */
sfs_block_ptr *balloc(size_t count);

/**
 * Frees blocks in the filesystem.
 *
 * The data associated with the blocks is not zeroed out! This just marks the
 * blocks as free, so that subsequent calls to balloc will use them. If you
 * require that the blocks be zeroed out, then this should be done right after
 * allocating them.
 */
int bfree(sfs_block_ptr *blocks, size_t count);

/**
 * Allocates a new inode in the filesystem.
 *
 * The new inode is uninitialized! All this function does is mark the next free
 * inode as used, and returns its number.
 */
sfs_inode_n ialloc();

/**
 * Frees inodes in the filesystem.
 *
 * The data blocks associated with an inode must be freed by the caller before
 * freeing an inode, otherwise those blocks will remain marked "used", but will
 * be unreachable by any file.
 */
int ifree(sfs_inode_n *inodes, size_t count);

/**
 * Creates a basic inode data structure in memory.
 */
struct sfs_inode *new_inode(sfs_inode_n n, sfs_mode mode);

/**
 * Dumps the contents of an inode to a human readable string.
 */
char * dump_inode(struct sfs_inode *inode);

#endif
