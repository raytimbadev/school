#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#define SPOOL_SHM_NAME "/comp-310-print-spool"
#define JOBS_SHM_NAME "/comp-310-print-jobs"
#define MSG_MAX_SIZE 100

#define SHM_MODE ((mode_t)0600)

struct PrintJob
{
    /**
     * The duration of the print job, in seconds.
     */
    int duration;

    /**
     * The contents of the print job to print.
     */
    char contents[MSG_MAX_SIZE];

    /**
     * Represent whether this job slot is available. When jobs are printed,
     * this is set to 0; when they are created, this is set to a nonzero value.
     */
    char available;

    /**
     * An identifier for the client.
     */
    int client_id;

    /**
     * The slot that the job was written to.
     */
    int slot_no;
};

/**
 * A convenient type synonym for the jobs list.
 */
typedef struct PrintJob * Jobs;

/**
 * The Spool type merely holds the metadata about the print jobs and the
 * semaphores.
 */
struct Spool
{
    /**
     * The position of the head of the printer within the queue.
     *
     * Dequeue operations on the spool block if jobs[head_index] is
     * available.
     *
     * When dequeue operations complete, head_index is increased by one, modulo
     * size .
     */
    int head_index;

    /**
     * The position of the tail of the printer
     *
     * Enqueue operations on the spool block if jobs[tail_index] is
     * unavailable.
     *
     * When enqueue operations complete, tail_index is increased by one, modulo
     * size.
     */
    int tail_index;

    /**
     * The maximum number of jobs in the spool.
     */
    int size;

    /**
     * A counter for the number of jobs printed in this spool.
     *
     * Clients use this number to determine the identifier for new jobs to
     * submit.
     */
    int job_counter;

    /**
     * A counter for the number of attached printers.
     */
    int printer_counter;

    /**
     * The lock on the print jobs held by the pool.
     */
    sem_t enqueue_lock;

    /**
     * The lock used to block the server when the spool empties.
     */
    sem_t dequeue_lock;

    /**
     * The lock used to block the for access to the jobs.
     */
    sem_t jobs_lock;
};

/**
 * A simple aggregate of the both the spool and jobs buffer.
 */
struct SpoolData
{
    struct Spool *spool;
    Jobs jobs;
};

/**
 * Creates a new print spool.
 */
void spool_create(struct SpoolData **data, int size);

/**
 * Get a print spool.
 *
 * If no spool server is running, the return value is NULL.
 */
void spool_get(struct SpoolData **data);

/**
 * Attach a printer to a spool.
 *
 * A new printer identifier is returned.
 */
int spool_attach_printer(struct Spool *spool);

/**
 * Get the PrintJob slot at the head of the spool.
 */
struct PrintJob *spool_head(struct SpoolData *data);

/**
 * Get the PrintJob slot at the tail of the spool.
 */
struct PrintJob *spool_tail(struct SpoolData *data);

/**
 * Destroys a print spool.
 */
int spool_destroy(struct SpoolData *data);

/**
 * Enqueues a print job to a spool, blocking until space becomes available in
 * the spool.
 *
 * A copy of the created print job is returned.
 */
struct PrintJob spool_enqueue_job(struct SpoolData *data, int duration, char *text);

/**
 * Dequeues a print job from a spool, or blocks until a job arrives in the
 * spool.
 */
struct PrintJob spool_dequeue_job(struct SpoolData *data);

#endif
