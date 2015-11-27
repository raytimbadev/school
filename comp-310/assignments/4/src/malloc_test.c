#include <stddef.h>

#include "jmalloc.h"

int main(int argc, char **argv)
{
    unsigned int i = 0;
    const unsigned int limit = 513; //1024 * 1024 * 1024; // 1 MiB
    unsigned int increments[5] = {
        512,
        768,
        1536,
        2048,
        4
    };

    void *ptrs[5] = {NULL, NULL, NULL, NULL, NULL};

    for(i = 0; i < limit; i += increments[i % 5])
    {
        ptrs[i % 5] = jmalloc(increments[i % 5]);
        jfree(ptrs[i % 5]);
    }
}
