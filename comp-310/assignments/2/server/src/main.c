#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#include "printspool.h"

#define USEC_PER_SEC 1000000

struct SpoolData *data = NULL;

void signal_handler(int signo)
{
    fprintf(stderr, "shutting down.");

    if(signo == SIGINT || signo == SIGSEGV) 
    {
        if(data != NULL)
            spool_destroy();
        exit(EXIT_SUCCESS);
    }

    fprintf(stderr, "Unhandled signal %s\n", strsignal(signo));
    signal(SIGINT, signal_handler);
}

int main(int argc, char **argv)
{
    if(argc != 2)
    {
        fprintf(stderr, "usage: %s <spool size>\n", argv[0]);
        exit(EXIT_FAILURE);
    }


    int size = atoi(argv[1]);
    struct PrintJob job;

    spool_create(&data, size);

    int character_duration, i, len;

    if(
            signal(SIGINT, signal_handler) == SIG_ERR 
            ||
            signal(SIGSEGV, signal_handler) == SIG_ERR
    ) {
        fprintf(stderr,
                "Unable to catch signals."
                "Graceful termination would be impossible. Terminating.");
        exit(EXIT_FAILURE);
    }

    const int id = spool_attach_printer(data->spool);

    printf("Attached printer %d.\n", id);

    while(1)
    {
        // Remove a job from the queue or block until one arrives.
        job = spool_dequeue_job(data);

        fprintf(stderr, "Got job %d.\nPrinting... (%d seconds)\n", 
                job.client_id,
                job.duration);

        len = strnlen(job.contents, MSG_MAX_SIZE);
        character_duration = 
            (int)(job.duration / (double)len * USEC_PER_SEC);

        for(i = 0; i < len; i++)
        {
            printf("%c", job.contents[i]);
            fflush(stdout);
            usleep(character_duration);
        }

        printf("\n");
    }

    return EXIT_SUCCESS;
}
