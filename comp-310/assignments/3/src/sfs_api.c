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
    {
        if(FILES[i] == NULL)
            continue;
        else
        {
            file_close(FILES[i]);
            FILES[i] = NULL;
        }
    }

    // Default is to allocate 1MiB in 512B blocks in a file called "sfs.bin".
    return sfs_init(fresh, "sfs.bin", (size_t)512, (size_t)2048);
}

int
sfs_get_next_filename(
        const char *path,
        char *filename,
        struct sfs_dir_iter **loc)
{
    fprintf(stderr,
            "SFS: sfs_get_next_filename: path '%s'.\n",
            path);

    char *mpath = strdup(path);

    if(*loc == SFS_DIR_START)
    {
        char *dirname = mpath;

        struct sfs_inode *dir_inode = NULL;

        fprintf(stderr,
                "SFS: get_next_filename: following from directory '%s'.\n",
                dirname);
        if(follow_path(
                dirname,
                &dir_inode,
                NULL,
                SFS_NO_MODE) < 0)
        {
            free(mpath);
            fprintf(stderr, "SFS: get_next_filename: follow_path failed.\n");
            return -1;
        }

        if(listdir(dir_inode, loc) != 0)
        {
            free(mpath);
            free(dir_inode);
            fprintf(stderr, "SFS: get_next_filename: listdir failed.\n");
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
    char *mpath = malloc(strlen(path) + 5);

    if(path[0] != '/')
    {
        fprintf(stderr,
                "SFS: sfs_get_file_size: path '%s' not absolute. Fixing.\n",
                path);
        mpath[0] = '/';
        strcpy(mpath + 1, path);
    }
    else
        strcpy(mpath, path);


    if(follow_path(mpath, &inode, NULL, SFS_NO_MODE) < 0)
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
    char *mpath = malloc(strlen(path) + 5);

    if(path[0] != '/')
    {
        fprintf(stderr,
                "SFS: sfs_remove: path '%s' not absolute. Fixing\n",
                path);
        mpath[0] = '/';
        strcpy(mpath + 1, path);
    }
    else
        strcpy(mpath, path);

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
            SFS_NO_MODE) < 0)
        return -1;

    int ret = link_remove(dir_inode, filename) >= 0;

    free(mpath);
    free(dir_inode);

    return ret;
}

file_id
sfs_fopen(const char *path)
{
    char *mpath = malloc(strlen(path) + 5);

    if(path[0] != '/')
    {
        fprintf(stderr,
                "SFS: sfs_fopen: path '%s' not absolute. Fixing\n",
                path);
        mpath[0] = '/';
        strcpy(mpath + 1, path);
    }
    else
        strcpy(mpath, path);

    struct sfs_inode *inode = NULL;

    if(follow_path(
            mpath,
            &inode,
            NULL,
            SFS_DEFAULT_MODE) < 0)
    {
        fprintf(stderr,
                "SFS: sfs_fopen: failed to follow path %s.\n",
                mpath);
        return -1;
    }

    file_id fd;

    int i;
    for(i = 0; i < MAX_OPEN_FILES; i++)
    {
        if(FILES[i] == NULL)
            continue;

        if(FILES[i]->inode.n == inode->n)
        {
            fprintf(stderr,
                    "SFS: sfs_fopen: trying to open already open file with "
                    "inode %u.\n",
                    inode->n);
            free(inode);
            return -1;
        }
    }

    if((fd = next_file_id()) == SFS_NO_MORE_FILES)
    {
        fprintf(stderr, "SFS: sfs_fopen: file limit reached.\n");
        free(inode);
        return -1;
    }

    fprintf(stderr,
            "SFS: sfs_fopen: allocated file descriptor %d.\n",
            fd);

    if(file_open(inode, &FILES[fd]) < 0)
    {
        fprintf(stderr, "SFS: sfs_fopen: file_open failed.\n");
        free(inode);
        return -1;
    }

    return fd;
}

int
sfs_fclose(file_id fd)
{
    if(FILES[fd] == NULL)
    {
        fprintf(stderr,
                "SFS: sfs_fclose: invalid file descriptor. %d\n",
                fd);
        return -1;
    }

    file_close(FILES[fd]);
    FILES[fd] = NULL;

    fprintf(stderr,
            "SFS: sfs_fclose: closed file %d.\n",
            fd);

    return 0;
}

int
sfs_fflush(file_id fd)
{
    if(FILES[fd] == NULL)
    {
        fprintf(stderr,
                "SFS: sfs_fflush: invalid file descriptor %d.\n",
                fd);
        return -1;
    }

    return file_flush(FILES[fd]);
}

int
sfs_fwrite(file_id fd, const char *buf, size_t length)
{
    if(FILES[fd] == NULL)
    {
        fprintf(stderr,
                "SFS: sfs_fwrite: invalid file descriptor %d.\n",
                fd);
        return -1;
    }

    return file_write(FILES[fd], buf, length);
}

int
sfs_fread(file_id fd, char *buf, size_t length)
{
    if(FILES[fd] == NULL)
    {
        fprintf(stderr,
                "SFS: sfs_fread: invalid file descriptor %d.\n",
                fd);
        return -1;
    }

    return file_read(FILES[fd], buf, length);
}

int
sfs_fseek(file_id fd, int offset, sfs_seek_origin origin)
{
    if(FILES[fd] == NULL)
    {
        fprintf(stderr,
                "SFS: sfs_fseek invalid file descriptor %d.\n",
                fd);
        return -1;
    }

    return file_seek(FILES[fd], offset, origin);
}
