#ifndef SFS_API_H
#define SFS_API_H

#include <stddef.h>

/**
 * Filesystem layout:
 * superblock, inode table, data blocks, inode bitmap, data bitmap
 */

// COMPILE-TIME PARAMETERS //

#ifndef SFS_WRITEBUF_SIZE
#define SFS_WRITEBUF_SIZE 1024
#endif

#ifndef MAGIC
#define MAGIC 0xAABB0005
#endif

#ifndef SFS_DIRECT_PTR_COUNT
#define SFS_DIRECT_PTR_COUNT 12
#endif

#ifndef SFS_BLOCK_SIZE
#define SFS_BLOCK_SIZE ((size_t)512)
#endif

#ifndef SFS_INODE_ALLOC_HEURISTIC
#define SFS_INODE_ALLOC_HEURISTIC 100
#endif

#ifndef MAXFILENAME
#define MAXFILENAME 20
#endif

// CONSTANTS //

#define SFS_DIR_START NULL

#define SFS_SUPERBLOCK_COUNT ((size_t)1)

#define SFS_REUSE 0
#define SFS_FRESH 1

#define MODE_O_R 0x0000000001
#define MODE_O_W 0x0000000010
#define MODE_O_X 0x0000000100
#define MODE_G_R 0x0000001000
#define MODE_G_W 0x0000010000
#define MODE_G_X 0x0000100000
#define MODE_A_R 0x0001000000
#define MODE_A_W 0x0010000000
#define MODE_A_X 0x0100000000
#define MODE_D   0x1000000000

#define SFS_NULL ((sfs_block_ptr)0)
#define SFS_INODE_NULL ((sfs_inode_n)0)

// TYPES //

typedef unsigned short file_id;

typedef unsigned short sfs_mode;
typedef unsigned short sfs_link_count;
typedef unsigned short sfs_uid;
typedef unsigned short sfs_gid;
typedef unsigned int sfs_block_ptr;
typedef unsigned int sfs_inode_n;

typedef unsigned int sfs_magic;

struct sfs_inode
{
    /**
     * The number of this inode, which is its position within the inode table.
     */
    sfs_inode_n n;

    /**
     * The permissions of the file represented by the inode.
     */
    sfs_mode mode;

    /**
     * The number of directory entries pointing to this inode.
     */
    sfs_link_count link_count;

    /**
     * The id of the creator of the inode.
     */
    sfs_uid uid;

    /**
     * The id of the group of the inode.
     */
    sfs_gid gid;

    /**
     * The size of the file represented by the inode.
     */
    size_t size;

    /**
     * The direct block pointers holding file data. The block pointer after the
     * last used direct block pointer, if any, has the special value SFS_NULL.
     */
    sfs_block_ptr direct_blocks[SFS_DIRECT_PTR_COUNT];

    /**
     * The indirect block pointer holding more file data. If no indirect block
     * is in use, then this attribute has the special value SFS_NULL.
     *
     * The indirect block is itself merely a 0-terminated array of
     * sfs_block_ptr values.
     */
    sfs_block_ptr indirect_block;
};

/**
 * Represents the on-disk structure of the superblock.
 */
struct sfs_superblock
{
    /**
     * The magic number for the SFS filesystem type.
     */
    sfs_magic magic;

    /**
     * The size of blocks on disk.
     */
    size_t block_size;

    /**
     * The number of blocks used for data in the system.
     */
    size_t block_count;

    /**
     * The number of blocks used as inodes.
     */
    size_t inode_blocks;

    /**
     * The number of blocks used for the free inode list.
     */
    size_t inode_bitmap_count;

    /**
     * The number of blocks used for the free data block list.
     */
    size_t block_bitmap_count;

    /**
     * The inode number of the root directory.
     */
    sfs_inode_n root;
};

typedef enum
{
    SFS_START,
    SFS_END,
    SFS_HERE
} sfs_seek_origin;

struct sfs_dir_entry
{
    sfs_inode_n inode;
    char filename[MAXFILENAME];
};

struct sfs_dir_iter
{
    struct sfs_dir_entry *entries;
    size_t size;
    int position;
};

/**
 * Represents a file on disk for random-access reading and writing.
 *
 * Writes are buffered internally. The write buffer is flushed to disk under
 * the following circumstances:
 *
 * * A read is performed.
 * * A seek is performed.
 * * A write would overflow the buffer.
 * * The buffer capacity is shrunk to less than its current size.
 * * The file is closed.
 * * An explicit flush is requested.
 *
 * If the write buffer is empty, but a write would overflow it, then the write
 * will fail. The buffer should be resized before trying again.
 */
struct sfs_file
{
    /**
     * The inode of the file, holding metadata about it.
     */
    struct sfs_inode *inode;

    /**
     * The offset within the file. This is modified when seeking.
     */
    unsigned int file_offset;

    /**
     * The offset within the buffer. This value increases when writes are
     * performed and resets to zero when the buffer is flushed.
     */
    unsigned int buf_offset;

    /**
     * The write buffer.
     */
    void *buf;

    /**
     * The size of the buffer.
     */
    size_t buf_size;
};

// SFS API //

/**
 * Formats a file in the host file system to SFS.
 *
 * The `fresh` argument should be one of the following symbolic constants:
 *
 * SFS_REUSE: 
 *     Reopens an existing filesystem; if there is no file identified by the
 *     given path, then an error occurs and -1 is returned.
 * SFS_FRESH:
 *     Creates a fresh filesystem. If the file exists then it is formatted,
 *     erasing its contents. If it does not exist it is created.
 */
int mksfs(int fresh);

/**
 * Gets the name of the next file in a directory.
 *
 * The file name will copied into the string pointed to by `fname.
 * The `loc` pointer is used to maintain state between calls to this function.
 * Upon the function's first use, it should point to the symbolic constant
 * SFS_DIR_START.
 *
 * If the
 *
 * The function will return zero if and only if there are no more files in the
 * directory.
 */
int sfs_get_next_filename(const char *path, char *fname, struct sfs_dir_iter **loc);

/**
 * Gets the size of the file identified by a path.
 */
int sfs_get_file_size(const char *path);

/**
 * Opens a file identified by a path.
 *
 * A new empty file is created if it does not exist. If the file does exist,
 * then it is opened for appending.
 *
 * A unique file descriptor is returned to identify the file.
 */
file_id sfs_fopen(const char *path);

/**
 * Closes a file.
 *
 * The return value is 0 only if the operation succeeds.
 */
int sfs_fclose(file_id fd);

/**
 * Flushes any dirty write buffers for a file to disk.
 *
 * Returns the number of bytes written by the flush operation.
 */
int sfs_fflush(file_id fd);

/**
 * Writes a given number of bytes from a given buffer to an open file.
 *
 * The number of bytes written is returned, or -1 if an error occurs.
 */
int sfs_fwrite(file_id fd, const char *buf, size_t length);

/**
 * Reads at most a given number of bytes from a file, copying them into a
 * given buffer.
 *
 * The number of bytes read is returned.
 */
int sfs_fread(file_id file, char *buf, size_t length);

/**
 * Seeks in a file by an offset from some origin.
 *
 * The return value is the number of bytes traversed, or -1 in case of error.
 */
int sfs_fseek(file_id file, int offset, sfs_seek_origin origin);

/**
 * Removes a file identified by a path from the filesystem.
 *
 * An error occurs if the file does not exist. The return value is -1 in this
 * case.
 *
 * Otherwise, the function succeeds and will return 0.
 */
int sfs_remove(const char *file);

#endif
