#include "jobs.h"

#include <sys/wait.h>
#include <string.h>
#include <stdio.h>

struct JobSpec * create_job(Jobs *j)
{
    struct JobSpec * new_job = malloc(sizeof(struct JobSpec));
    struct Node * current = NULL;
    struct JobSpec * item = NULL;
    int max_id = 0;

    for(current = j->first; current != NULL; current = current->next)
    {
        item = (struct JobSpec *)current->data;
        if(max_id < item->id)
            max_id = item->id;
    }

    new_job->id = max_id + 1;
    new_job->pid = 0;
    new_job->return_code = -1;
    new_job->bg = 0;
    new_job->running = 1;
    new_job->command = NULL;

    return new_job;
}

int add_job(Jobs *jobs, struct JobSpec *j)
{
    struct Node * current = NULL;
    struct JobSpec * item = NULL;

    for(current = jobs->first; current != NULL; current = current->next)
    {
        item = (struct JobSpec *)current->data;
        if(item->id == j->id)
            return 1;
    }

    // if we make it here, then the id is not in use.
    ll_push_front(jobs, j);

    return 0;
}

struct JobSpec * remove_job(Jobs *jobs, int id)
{
    struct Node * current = jobs->first, *prev = NULL;
    struct JobSpec * item = NULL;

    while(current != NULL)
    {
        item = (struct JobSpec *)current->data;
        if(item->id == id)
            break;
        else
            item = NULL;
    }

    // if item is NULL, then we didn't find a match
    if(item == NULL)
        return NULL;

    // if current is NULL, then we're at the end of the list, so we'll cheat
    // and use ll_pop_back
    // TODO refactor because ll_pop_back is O(n) and we have enough context
    // here to do the remove in O(1)
    if(current == NULL)
        return (struct JobSpec *)ll_pop_back(jobs);

    // if prev is NULL, then we're at the front, so we'll just use ll_pop_front
    // This is legit because ll_pop_front is O(1).
    if(prev == NULL)
        return (struct JobSpec *)ll_pop_front(jobs);

    // otherwise, we're somewhere in the middle, so we have to stitch the
    // sublists back together, omitting the element we found.
    prev->next = current->next;

    // Free the memory of the node structure.
    ll_free_node(current);

    // decrease the size of the jobs list by one.
    jobs->size--;

    // return the item we found.
    return item;
}

void destroy_job(struct JobSpec *j)
{
    free(j->command);
    free(j);
}

void destroy_job_in(Jobs *jobs, struct Node *prev, struct Node *current)
{
    // remove the current item from the list.
    // if we're at the front of the list
    if(prev == NULL)
        jobs->first = current->next;
    else
        prev->next = current->next;

    destroy_job((struct JobSpec *)current->data);
    ll_free_node(current);
    jobs->size--;
}

void check_jobs(Jobs *jobs, int show)
{
    struct Node *prev = NULL, *current = NULL;
    struct JobSpec *j = NULL;
    char *status = NULL;
    int pstat, pid;
    int asprintf_result;

    for(current = jobs->first; current != NULL; current = current->next)
    {
        asprintf_result = 0;
        j = (struct JobSpec *)current->data;
        pid = waitpid(j->pid, &pstat, WNOHANG);

        if(j->running == 0)
            destroy_job_in(jobs, prev, current);
        else if(pid == j->pid)
        {
            j->running = 0; // the subprocess terminated.
            if(WIFEXITED(pstat))
            {
                j->return_code = WEXITSTATUS(pstat);

                asprintf_result =
                    asprintf(&status, "done (%3d)", j->return_code);

            }
            else if(WIFSIGNALED(pstat))
            {
                j->return_code = -1;

                int sig = WTERMSIG(pstat);

                asprintf_result = asprintf(&status, "SIG%s", strsignal(sig));
            }
            else
                status = strdup("unknown");

            if(asprintf_result < 0)
                printf("[%d] %5d error: unable to get status\n", j->id,
                        j->pid);
            else if(show == JOBS_SHOW)
            {
                printf("[%d] %5d %10s %s", j->id, j->pid, status,
                        j->command);
                free(status);
            }

            destroy_job_in(jobs, prev, current);
        }
        else
            prev = current;
    }
}
