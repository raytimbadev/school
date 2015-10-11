#include "printspool.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

void spool_create(struct SpoolData **data, int size)
{
    int i, shm_fd, jobs_shm_fd;

    // Create the shared memory object
    shm_fd = shm_open(
            SPOOL_SHM_NAME,
            O_RDWR | O_CREAT | O_EXCL,
            SHM_MODE);

    if(shm_fd == -1)
    {
        if(errno == EEXIST)
        {
            spool_get(data);
            if(*data == NULL)
                return;

            fprintf(stderr, "Opened existing spool.\n");
            if((*data)->spool->size < size)
                fprintf(stderr, 
                        "Warning: existing spool has size %d < %d.\n",
                        (*data)->spool->size,
                        size);
            return;
        }

        fprintf(stderr, "Error opening shared memory: %s\n", strerror(errno));
        exit(1);
    }

    // Create a shared memory object for the jobs array
    jobs_shm_fd = shm_open(
            JOBS_SHM_NAME,
            O_RDWR | O_CREAT | O_TRUNC,
            SHM_MODE);

    if(jobs_shm_fd == -1)
    {
        shm_unlink(SPOOL_SHM_NAME);
        fprintf(stderr,
                "Error opening jobs shared memory: %s\n",
                strerror(errno));
        exit(1);
    }

    struct Spool *spool_map;
    Jobs jobs;

    const int spool_shm_size = sizeof(*spool_map);
    const int jobs_shm_size = sizeof(*jobs) * size;

    ftruncate(shm_fd, spool_shm_size);
    ftruncate(jobs_shm_fd, jobs_shm_size);

    // Map the shared memory for the spool into this process.
    spool_map = 
        mmap(0, spool_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);

    if(spool_map == MAP_FAILED)
    {
        fprintf(stderr, "spool mmap failed\n");
        exit(EXIT_FAILURE);
    }

    spool_map->size = size;

    // Map the shared memory for the jobs into this process
    jobs = mmap(0, jobs_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED,
            jobs_shm_fd, 0);

    if(jobs == MAP_FAILED)
    {
        fprintf(stderr, "jobs mmap failed\n");
        exit(EXIT_FAILURE);
    }

    // Initialize the semaphores in the spool
    sem_init(&spool_map->enqueue_lock, 1, size);
    sem_init(&spool_map->dequeue_lock, 1, 0);
    sem_init(&spool_map->jobs_lock, 1, 1);

    // Set up the jobs
    for(i = 0; i < size; i++)
    {
        jobs[i].available = 1;
        jobs[i].duration = 0;
        jobs[i].contents[0] = '\0';
    }

    // Set up the printer locations
    spool_map->head_index = 0;
    spool_map->tail_index = 0;

    fprintf(stderr, "Created new spool with %d slots.\n", size);

    if(*data == NULL)
        *data = malloc(sizeof(*data));

    (*data)->spool = spool_map;
    (*data)->jobs = jobs;

    return;
}

void spool_destroy()
{
    shm_unlink(SPOOL_SHM_NAME);
    shm_unlink(JOBS_SHM_NAME);
}

void spool_get(struct SpoolData **data)
{
    int shm_fd, jobs_shm_fd;
    struct Spool *spool_map;
    Jobs jobs;

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

    if(spool_map == MAP_FAILED)
    {
        fprintf(stderr, "spool mmap failed: %s\n", strerror(errno));
        exit(1);
    }

    const int jobs_shm_size = sizeof(*jobs) * spool_map->size;

    // Map the shared memory for the jobs into this process
    jobs = mmap(0, jobs_shm_size, PROT_READ | PROT_WRITE, MAP_SHARED,
            jobs_shm_fd, 0);

    if(jobs == MAP_FAILED)
    {
        fprintf(stderr, "jobs mmap failed: %s\n", strerror(errno));
        exit(1);
    }

    if(*data == NULL)
        *data = malloc(sizeof(*data));

    (*data)->spool = spool_map;
    (*data)->jobs = jobs;
}

int spool_attach_printer(struct Spool *spool)
{
    sem_wait(&spool->jobs_lock);

    int id = spool->printer_counter;
    spool->printer_counter ++;

    sem_post(&spool->jobs_lock);

    return id;
}

struct PrintJob *spool_head(struct SpoolData *data)
{
    return &data->jobs[data->spool->head_index];
}

struct PrintJob *spool_tail(struct SpoolData *data)
{
    return &data->jobs[data->spool->tail_index];
}

struct PrintJob spool_enqueue_job(struct SpoolData *data, int duration, char *text)
{
    // Acquire a lock on the spool and require an enqueue.
    sem_wait(&data->spool->enqueue_lock);
    sem_wait(&data->spool->jobs_lock);

    // Our semaphores guarantee that the next printing slot is available, but
    // we assert it here as a precaution.
    assert(spool_tail(data)->available != 0);

    // Copy the print job into the spool.
    spool_tail(data)->available = 0;
    strncpy(spool_tail(data)->contents, text, MSG_MAX_SIZE - 1);
    spool_tail(data)->duration = duration;
    spool_tail(data)->client_id = data->spool->job_counter;
    spool_tail(data)->slot_no = data->spool->tail_index;

    // Copy the print job into a local so we can return it later.
    struct PrintJob job = *spool_tail(data);

    // Do some bookkeeping
    data->spool->tail_index ++; // So the next job won't overwrite ours
    data->spool->tail_index %= data->spool->size; // Our FIFO is a round-robin
    data->spool->job_counter ++; // So the next job has a new identifier

    // Release the lock on the spool and signal that a dequeue is necessary.
    sem_post(&data->spool->jobs_lock);
    sem_post(&data->spool->dequeue_lock);

    return job;
}

struct PrintJob spool_dequeue_job(struct SpoolData *data)
{
    fprintf(stderr, "dequeuing...\n");
    // Acquire a lock on the spool and require a dequeue
    sem_wait(&data->spool->dequeue_lock);
    sem_wait(&data->spool->jobs_lock);

    // Our semaphores guarantee that the next printing slot should be full, but
    // we assert it here as a precaution.
    assert(spool_head(data)->available == 0);

    // Copy the print job into a local
    struct PrintJob job = *spool_head(data);

    // Mark the original as available.
    spool_head(data)->available = 1;

    // Do some bookkeeping
    data->spool->head_index ++; // Move the reader index forwards by one, mod size
    data->spool->head_index %= data->spool->size;

    // Release the lock on the spool and signal that another enqueue may be
    // performed.
    sem_post(&data->spool->jobs_lock);
    sem_post(&data->spool->enqueue_lock);

    return job;
}
