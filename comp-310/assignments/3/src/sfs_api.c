#include <stdio.h>
#include <stdlib.h>

#include "sfs_api.h"
#include "disk_emu.h"
#include "sfs_helpers.h"

int
mksfs(int fresh)
{
    // Default is to allocate 1MiB in 512B blocks in a file called "sfs.bin".
    return sfs_format(fresh, "sfs.bin", (size_t)512, (size_t)2048);
}

int sfs_get_next_filename(const char * filename, dirloc *loc)
{
    return 0;
}
