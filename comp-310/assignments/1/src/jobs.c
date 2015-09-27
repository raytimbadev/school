#include "jobs.h"

struct JobSpec * create_job(Jobs *j)
{
    struct JobSpec * new_job = malloc(sizeof(struct JobSpec));
    struct Node * current = j->first;
    struct JobSpec * item = NULL;
    int id = 1;

    while(current != NULL)
    {
        item = (struct JobSpec *)current->data;
        if(id == item->id)
            id++;
        current = current->next;
    }

    new_job->id = id;
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
            return 1; // id already in use
    }

    // if we make it here, then the id is not in use.
    ll_push_back(jobs, j);

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
