#include <stdio.h>
#include <stdlib.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "printspool.h"

int main(int argc, char **argv)
{
    if(argc != 3)
    {
        fprintf(stderr, "usage: %s <duration> <message>\n", argv[0]);
        exit(1);
    }

    int duration = atoi(argv[1]);
    char *message = argv[2];

    struct Spool *spool = spool_get();

    spool_enqueue_job(spool, duration, message);

    printf("Message sent.\n");
    return EXIT_SUCCESS;
}

