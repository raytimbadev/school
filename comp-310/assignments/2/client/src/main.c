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
    struct SpoolData *data = NULL;

    spool_get(&data);

    struct PrintJob job = spool_enqueue_job(data, duration, message);

    printf("Enqueued job #%d to slot %d.\n", job.client_id, job.slot_no);

    free(data);

    return EXIT_SUCCESS;
}

