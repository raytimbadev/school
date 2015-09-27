#include "builtins.h"
#include "history.h"

#include <stdio.h>
#include <stdlib.h>

int builtin_history(History *h)
{
    print_history(h);
    return 0;
}

int builtin_r(History *h, char *prefix)
{
    fprintf(stderr, "STUB: builtin_r\n");
    return -1;
}

int builtin_cd(char *dst)
{
    fprintf(stderr, "STUB: builtin_cd\n");
    return -1;
}

int builtin_pwd()
{
    fprintf(stderr, "STUB: builtin_pwd\n");
    return -1;
}

int builtin_jobs(Jobs *j)
{
    fprintf(stderr, "STUB: builtin_jobs\n");
    return -1;
}

int builtin_fg(Jobs *j, int id)
{
    fprintf(stderr, "STUB: builtin_fg\n");
    return -1;
}
