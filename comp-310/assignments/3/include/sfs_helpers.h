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
char * block_malloc(const struct sfs_superblock *superblock, size_t block_count);

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
 */
int follow_path(
        char *path,
        struct sfs_inode **inode,
        struct sfs_inode *parent);

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
 */
int file_open(const char *path, struct sfs_file **file);

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
 *
 * Returns the number of bytes flushed to disk, which is at least zero.
 */
int file_write(struct sfs_file *file, void *buf, size_t count);

/**
 * Seeks to a given offset from the start of the file.
 */
int file_seek(struct sfs_file *file, unsigned int offset);

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

#endif
