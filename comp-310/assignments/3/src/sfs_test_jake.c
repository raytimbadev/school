#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sfs_api.h"

int main(int argc, char **argv)
{
    mksfs(1);

    char filename[MAXFILENAME + 1];
    struct sfs_dir_iter *loc = NULL;

    file_id fd = sfs_fopen("/test.txt");

    char *buf = "Hello, world!";
    int buf_size = strlen(buf) + 1;

    fprintf(stderr, "TEST: writing: '%s'.\n", buf);
    sfs_fwrite(fd, buf, buf_size);

    sfs_fclose(fd);

    sfs_fopen("/test.txt");

    buf = malloc(buf_size);
    sfs_fread(fd, buf, buf_size);

    fprintf(stderr, "TEST: reading: '%s'.\n", buf);

    sfs_fclose(fd);

    loc = SFS_DIR_START;
    while(sfs_get_next_filename("/", filename, &loc))
        fprintf(stderr, "TEST: listing: %s.\n", filename);
    free_dir_iter(loc);

    fprintf(stderr,
            "TEST: deleting file.\n");

    sfs_remove("/test.txt");

    loc = SFS_DIR_START;
    while(sfs_get_next_filename("/", filename, &loc))
        fprintf(stderr, "TEST: listing: %s.\n", filename);
    free_dir_iter(loc);
}
