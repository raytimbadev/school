#include "builtins.h"
#include "history.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/wait.h>

extern struct JobSpec * evaluate(History *history, char *line);

int builtin_history(History *h)
{
    print_history(h);
    return 0;
}

int builtin_r(History *h, char *prefix)
{
    struct Node *node = NULL;
    struct HistoryItem *item = NULL;

    if(prefix == NULL)
    {
        node = h->first;
        if(node != NULL)
            item = (struct HistoryItem *)node->data;
    }
    else
        item = history_find_item(h, prefix);

    if(item == NULL)
    {
        if(prefix == NULL)
            fprintf(stderr, "history: no history\n");
        else
            fprintf(stderr, "history: no command with prefix: %s\n", prefix);

        return -1;
    }
    else
    {
        printf("%s", item->contents);
        struct JobSpec *job = evaluate(h, strdup(item->contents));
        return job->bg ? 0 : job->return_code;
    }
}

int builtin_cd(char *dst)
{
    int ret = chdir(dst);
    if(ret == -1)
        fprintf(stderr, "cd: error: %s\n", strerror(errno));
    return ret;
}

int builtin_pwd()
{
    char *cwd = getcwd(NULL, 0); // use glibc hax
    if(cwd == NULL)
    {
        fprintf(stderr, "pwd: error: %s\n", strerror(errno));
        return 1;
    }

    printf("%s\n", cwd);
    free(cwd);

    return 0;
}

int builtin_jobs(Jobs *j)
{
    struct Node *current = NULL;
    struct JobSpec *item = NULL;
    char *status = NULL;

    for(current = j->first; current != NULL; current = current->next)
    {
        item = (struct JobSpec *)current->data;
        status = item->running ? "running" : "done";
        printf("[%d] %5d %7s %s", item->id, item->pid, status, item->command);
    }

    check_jobs(j, JOBS_NO_SHOW);

    return 0;
}

int builtin_fg(Jobs *j, int id)
{
    struct Node *current = NULL;
    struct JobSpec *job = NULL;

    for(current = j->first; current != NULL; current = current->next)
    {
        job = (struct JobSpec *)current->data;
        if(id == JOBS_MOST_RECENT || id == job->id)
            break;
        else
            job = NULL;
    }

    if(job == NULL)
    {
        if(id == 0)
            fprintf(stderr, "fg: no current job\n");
        else
            fprintf(stderr, "fg: job not found: %d\n", id);
        return 1;
    }
    else
    {
        int pstat, pid;

        job->bg = 0;
        pid = waitpid(job->pid, &pstat, 0);

        if(pid == job->pid)
        {
            if(WIFEXITED(pstat))
            {
                job->return_code = WEXITSTATUS(pstat);
                job->running = 0;
            }
            else if(WIFSIGNALED(pstat))
            {
                job->return_code = -1;
                job->running = 0;
            }
        }

        return job->return_code;
    }
}
