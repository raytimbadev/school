#include <stdio.h>
#include <stdlib.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "printspool.h"

int main(int argc, char **argv)
{
    if(argc != 2)
    {
        fprintf(stderr, "usage: %s <spool size>\n", argv[0]);
        exit(1);
    }

    int size = atoi(argv[1]);
    struct Spool *spool = spool_create(size);
    struct PrintJob job;

    while(1)
    {
        job = spool_dequeue_job(spool);
        fprintf(stderr, "Got job. Printing... (%d seconds)\n", job.duration);
        sleep((unsigned int)job.duration);
        printf("Received: %s\n", job.contents);
    }

    return EXIT_SUCCESS;
}
