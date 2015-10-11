#include "printspool.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

struct Spool *spool_create(int size)
{
    int i, shm_fd, jobs_shm_fd;
    struct Spool *spool_map;

    // Create the shared memory object
    shm_fd = shm_open(
            SPOOL_SHM_NAME,
            O_RDWR | O_CREAT | O_TRUNC,
            SHM_MODE);

    printf("created spool shm\n");

    if(shm_fd == -1)
    {
        fprintf(stderr, "Error opening shared memory: %s\n", strerror(errno));
        exit(1);
    }

    // Create a shared memory object for the jobs array
    jobs_shm_fd = shm_open(
            JOBS_SHM_NAME,
            O_RDWR | O_CREAT | O_TRUNC,
            SHM_MODE);

    printf("created jobs shm\n");

    if(jobs_shm_fd == -1)
    {
        shm_unlink(SPOOL_SHM_NAME);
        fprintf(stderr,
                "Error opening jobs shared memory: %s\n",
                strerror(errno));
        exit(1);
    }

    const int spool_shm_size = sizeof(*spool_map);
    const int jobs_shm_size = sizeof(*spool_map->jobs) * size;

    ftruncate(shm_fd, spool_shm_size);
    ftruncate(jobs_shm_fd, jobs_shm_size);

    // Map the shared memory for the spool into this process.
    spool_map = 
        mmap(0, spool_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);

    if(spool_map == MAP_FAILED)
    {
        fprintf(stderr, "spool mmap failed\n");
        exit(1);
    }

    printf("mapped spool, size: %d\n", spool_shm_size);

    spool_map->size = size;

    // Map the shared memory for the jobs into this process
    spool_map->jobs =
        mmap(0, jobs_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, jobs_shm_fd,
                0);

    printf("mapped jobs, size: %d\n", jobs_shm_size);

    // Initialize the semaphores in the spool
    sem_init(&spool_map->enqueue_lock, 1, size);
    sem_init(&spool_map->dequeue_lock, 1, 0);
    sem_init(&spool_map->jobs_lock, 1, 1);

    printf("initialized semaphores\n");

    // Set up the jobs
    for(i = 0; i < size; i++)
    {
        spool_map->jobs[i].available = 1;
        spool_map->jobs[i].duration = 0;
        spool_map->jobs[i].contents[0] = '\0';
    }

    // Set up the printer locations
    spool_map->head_index = 0;
    spool_map->tail_index = 0;

    return spool_map;
}

struct Spool *spool_get()
{
    int shm_fd, jobs_shm_fd;
    struct Spool *spool_map;

    shm_fd = shm_open(
            SPOOL_SHM_NAME,
            O_RDWR,
            SHM_MODE);

    if(shm_fd == -1)
    {
        fprintf(stderr, "Error opening spool shared memory: %s\n",
                strerror(errno));
        exit(1);
    }

    // Create a shared memory object for the jobs array
    jobs_shm_fd = shm_open(
            JOBS_SHM_NAME,
            O_RDWR,
            SHM_MODE);

    if(shm_fd == -1)
    {
        fprintf(stderr,
                "Error opening jobs shared memory: %s\n",
                strerror(errno));
        exit(1);
    }

    const int spool_shm_size = sizeof(*spool_map);

    // Map the shared memory for the spool into this process.
    spool_map = 
        mmap(0, spool_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd,
                0);

    printf("mapped spool, size: %d\n", spool_shm_size);


    if(spool_map == MAP_FAILED)
    {
        fprintf(stderr, "spool mmap failed: %s\n", strerror(errno));
        exit(1);
    }

    const int jobs_shm_size = sizeof(*spool_map->jobs) * spool_map->size;

    // Map the shared memory for the jobs into this process
    spool_map->jobs =
        mmap(0, jobs_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, jobs_shm_fd,
                0);

    if(spool_map->jobs == MAP_FAILED)
    {
        fprintf(stderr, "jobs mmap failed: %s\n", strerror(errno));
        exit(1);
    }

    printf("mapped jobs, size: %d\n", jobs_shm_size);

    return spool_map;
}

struct PrintJob *spool_head(struct Spool *spool)
{
    return &spool->jobs[spool->head_index];
}

struct PrintJob *spool_tail(struct Spool *spool)
{
    return &spool->jobs[spool->tail_index];
}

void spool_enqueue_job(struct Spool *spool, int duration, char *text)
{
    fprintf(stderr, "enqueuing job...\n");

    sem_wait(&spool->enqueue_lock);
    sem_wait(&spool->jobs_lock);

    assert(spool_tail(spool)->available != 0);

    // Copy the print job into the spool.
    spool_tail(spool)->available = 0;
    strncpy(spool_tail(spool)->contents, text, MSG_MAX_SIZE);
    spool_tail(spool)->duration = duration;
    fprintf(stderr, "Enqueued job \"%s\" (%d seconds) to slot %d.\n", 
            text, duration, spool->tail_index);

    spool->tail_index ++;
    spool->tail_index %= spool->size;

    sem_post(&spool->jobs_lock);
    sem_post(&spool->dequeue_lock);
}

struct PrintJob spool_dequeue_job(struct Spool *spool)
{
    fprintf(stderr, "dequeuing job... ");

    sem_wait(&spool->dequeue_lock);
    sem_wait(&spool->jobs_lock);

    struct PrintJob job = *spool_head(spool);
    spool_head(spool)->available = 1;

    fprintf(stderr, "Dequeued job \"%s\" (%d seconds) from slot %d.\n", 
            job.contents, job.duration, spool->head_index);

    spool->head_index ++;
    spool->head_index %= spool->size;

    sem_post(&spool->jobs_lock);
    sem_post(&spool->enqueue_lock);

    return job;
}
