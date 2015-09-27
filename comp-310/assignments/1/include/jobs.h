#ifndef JOBS_H
#define JOBS_H

#define _GNU_SOURCE

#include "ll.h"

#define JOBS_NO_SHOW 0
#define JOBS_SHOW 1
#define JOBS_MOST_RECENT (-1)

struct JobSpec
{
    int id; // the identifier for this job
    int pid; // the process identifier
    int return_code; // the return code of the process; meaningless until 
                     // running is false
    int bg; // whether the job is in the background
    int running; // whether the job is running (or completed)
    char *command;
};

typedef struct LinkedList Jobs;

/**
 * Create a new job within the context of a Jobs list.
 *
 * The JobSpec is *not* added to the Jobs list. See add_job.
 *
 * The data in the JobSpec will be initialized as follows:
 * id: the smallest nonzero natural number unused in the Jobs list
 * pid: 0
 * return_code: -1
 * bg: 0 (false)
 * running: 1 (true)
 * command: NULL
 */
struct JobSpec * create_job(Jobs *j);

/**
 * Adds a job with the given specification to a jobs list. If a job with the
 * same id as the given JobSpec exists in the jobs list, this function returns
 * a nonzero value and the job is not added. Zero is returned on success.
 */
int add_job(Jobs *jobs, struct JobSpec *j);

/**
 * Remove a job from a Jobs list by its id.
 * If a JobSpec can be found with the given id in the jobs list, then it is
 * returned, and the jobs list is altered to no longer contain that JobSpec.
 * If no JobSpec matches the given id, then this function returns NULL.
 */
struct JobSpec * remove_job(Jobs *jobs, int id);

/**
 * Free the memory used by a JobSpec.
 */
void destroy_job(struct JobSpec *j);

void check_jobs(Jobs *jobs, int show);

#endif
