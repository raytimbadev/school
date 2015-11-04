#include <stdio.h>
#include <stdlib.h>

#include "sfs_api.h"
#include "disk_emu.h"

static sfs_superblock SUPERBLOCK = NULL;

int
sfs_format(int fresh, char *path, size_t block_size, size_t block_count)
{
    unsigned int inodes_per_block = block_size / sizeof(struct sfs_inode);
    unsigned int inode_count =
        block_size * block_count / SFS_INODE_ALLOC_HEURISTIC;
    unsigned int inode_blocks = inode_count / inodes_per_block + 1;

    unsigned int bits_per_block = block_size * 8;
    unsigned int bitmap_blocks = block_count / bits_per_block + 1;

    unsigned int total_blocks = block_count + inode_blocks + bitmap_block + 1;

    // TODO check for errors
    if(fresh)
        if(init_fresh_disk(path, block_size, total_blocks) != 0)
            return -1;
    else
        if(init_disk(path, block_size, total_blocks) != 0)
            return -1;

    SUPERBLOCK = malloc(sizeof(*SUPERBLOCK));
    superblock->magic = MAGIC;
    superblock->block_size = block_size;
    superblock->block_count = block_count;
    superblock->inode_blocks = inode_blocks;

    return 0;
}

int
mksfs(int fresh)
{
    // Default is to allocate 1MiB in 512B blocks in a file called "sfs.bin".
    return sfs_format(fresh, "sfs.bin", (size_t)512, (size_t)2048);
}
