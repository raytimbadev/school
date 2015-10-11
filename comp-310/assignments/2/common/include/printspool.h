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

struct PrintJob {
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
};

struct Spool {
    /**
     * The first and last jobs in the print spool.
     */
    struct PrintJob *jobs;

    /**
     * The name of the shared memory object used to store the jobs array.
     */
    char *jobs_shm_name;

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
 * Creates a new print spool.
 */
struct Spool *spool_create(int size);

/**
 * Get a print spool.
 *
 * If no spool server is running, the return value is NULL.
 */
struct Spool *spool_get();

/**
 * Get the PrintJob slot at the head of the spool.
 */
struct PrintJob *spool_head(struct Spool *spool);

/**
 * Get the PrintJob slot at the tail of the spool.
 */
struct PrintJob *spool_tail(struct Spool *spool);

/**
 * Destroys a print spool.
 */
void spool_destroy(struct Spool *spool);

/**
 * Enqueues a print job to a spool, blocking until space becomes available in
 * the spool.
 */
void spool_enqueue_job(struct Spool *spool, int duration, char *text);

/**
 * Dequeues a print job from a spool, or blocks until a job arrives in the
 * spool.
 */
struct PrintJob spool_dequeue_job(struct Spool *spool);

#endif
