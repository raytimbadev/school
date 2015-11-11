#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sfs_api.h"
#include "disk_emu.h"
#include "sfs_helpers.h"

struct sfs_file *FILES[MAX_OPEN_FILES];

file_id
next_file_id()
{
    unsigned int i;
    for(i = 0; i < MAX_OPEN_FILES; i++)
        if(FILES[i] == NULL)
            return i;

    return SFS_NO_MORE_FILES;
}

int
mksfs(int fresh)
{
    int i;
    for(i = 0; i < MAX_OPEN_FILES; i++)
        FILES[i] = NULL;

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
        if(follow_path(mpath, &dir_inode, NULL, 0) != 0)
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
    if(follow_path(mpath, &inode, NULL, 0) != 0)
    {
        free(mpath);
        return -1;
    }

    const int size = inode->size;

    free(mpath);
    free(inode);
    return size;
}

int
sfs_remove(const char *path)
{
    if(path[0] != '/')
        return -1;

    char *mpath = strdup(path);

    int i = strlen(mpath);

    while(mpath[i] != '/') i--;

    mpath[i] = '\0';
    const char *filename = mpath + i + 1;
    char *dirname = mpath;

    struct sfs_inode *dir_inode = NULL;
    if(follow_path(
            dirname,
            &dir_inode,
            NULL,
            SFS_NO_MODE) != 0)
        return -1;

    int ret = link_remove(dir_inode, filename) >= 0;

    free(mpath);
    free(dir_inode);

    return ret;
}

file_id
sfs_fopen(const char *path)
{
    if(path[0] != '/')
        return -1;

    char *mpath = strdup(path);
    struct sfs_inode *inode = NULL;

    if(follow_path(
            mpath,
            &inode,
            NULL,
            SFS_DEFAULT_MODE) != 0)
        return -1;

    file_id fd;

    if((fd = next_file_id()) == SFS_NO_MORE_FILES)
        return -1;

    if(file_open(inode, &FILES[fd]) < 0)
        return -1;

    return fd;
}

int
sfs_fclose(file_id fd)
{
    if(FILES[fd] == NULL)
        return -1;

    file_close(FILES[fd]);
    FILES[fd] = NULL;
    return 0;
}

int
sfs_fflush(file_id fd)
{
    if(FILES[fd] == NULL)
        return -1;

    return file_flush(FILES[fd]);
}

int
sfs_fwrite(file_id fd, const char *buf, size_t length)
{
    if(FILES[fd] == NULL)
        return -1;

    return file_write(FILES[fd], buf, length);
}

int
sfs_fread(file_id fd, char *buf, size_t length)
{
    if(FILES[fd] == NULL)
        return -1;

    return file_read(FILES[fd], buf, length);
}

int
sfs_fseek(file_id fd, int offset, sfs_seek_origin origin)
{
    if(FILES[fd] == NULL)
        return -1;

    return file_seek(FILES[fd], offset, origin);
}
