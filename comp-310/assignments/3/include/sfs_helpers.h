#ifndef SFS_HELPERS_H
#define SFS_HELPERS_H

#include "sfs_api.h"
#include "disk_emu.h"

/**
 * Initializes an SFS disk.
 */
int sfs_format(int fresh, char *path, size_t block_size, size_t block_count);

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

#endif
