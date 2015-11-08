#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sfs_api.h"
#include "disk_emu.h"
#include "sfs_helpers.h"

int
mksfs(int fresh)
{
    // Default is to allocate 1MiB in 512B blocks in a file called "sfs.bin".
    return sfs_format(fresh, "sfs.bin", (size_t)512, (size_t)2048);
}

int
sfs_get_next_filename(
        const char *path,
        char *filename,
        struct sfs_dir_iter **loc)
{
    char *mpath = strdup(path);
    if(*loc == SFS_DIR_START)
    {
        struct sfs_inode *dir_inode = NULL;
        if(follow_path(mpath, &dir_inode, NULL) != 0)
        {
            free(mpath);
            return -1;
        }

        if(listdir(dir_inode, loc) != 0)
        {
            free(mpath);
            free(dir_inode);
            return -2;
        }

        free(mpath);
        free(dir_inode);

        return sfs_get_next_filename(path, filename, loc);
    }

    struct sfs_dir_iter *locp = *loc;

    if(locp->position == locp->size)
        return 0;

    strncpy(filename, locp->entries[locp->position].filename, MAXFILENAME);
    locp->position++;
    return locp->position;
}

int
sfs_get_file_size(const char *path)
{
    struct sfs_inode *inode = NULL;
    char *mpath = strdup(path);
    if(follow_path(mpath, &inode, NULL) != 0)
    {
        free(mpath);
        return -1;
    }

    const int size = inode->size;

    free(mpath);
    free(inode);
    return size;
}
