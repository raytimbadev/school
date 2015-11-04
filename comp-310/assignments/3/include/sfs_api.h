#ifndef SFS_API_H
#define SFS_API_H

#include <stddef.h>

#define SFS_DIR_START NULL

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

typedef int file_id;
typedef int * dirloc;

typedef unsigned short sfs_mode;
typedef unsigned short sfs_link_count;
typedef unsigned short sfs_uid;
typedef unsigned short sfs_gid;
typedef unsigned int sfs_block_ptr;
typedef unsigned int sfs_inode_n;

typedef unsigned int sfs_magic;

#define MAGIC ((magic)0xAABB0005)

#define SFS_NULL ((sfs_block_ptr)0)

#define SFS_DIRECT_PTR_COUNT 12

#define SFS_BLOCK_SIZE ((size_t)512)

#define SFS_INODE_ALLOC_HEURISITC 100

#define MAXFILENAME 20

struct sfs_inode
{
    sfs_mode mode;
    sfs_link_count link_count;
    sfs_uid uid;
    sfs_gid gid;
    size_t size;
    sfs_block_ptr direct_blocks[SFS_DIRECT_PTR_COUNT];
    sfs_block_ptr indirect_block;
};

struct sfs_superblock
{
    sfs_magic magic;
    size_t block_size;
    size_t block_count;
    size_t inode_blocks;
    sfs_inode_n root;
};

typedef enum
{
    SFS_START,
    SFS_END,
    SFS_HERE
} sfs_seek_origin;

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
 * The function will return zero if and only if there are no more files in the
 * directory.
 */
int sfs_get_next_filename(const char *fname, dirloc *loc);

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
