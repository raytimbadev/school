#include "repl.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>

int parse_command(char *line, char **args, int *background)
{
    int i = 0, j = 0, token_length = 0;
    char *token, *loc;
    //
    // Check if background is specified..
    // TODO check that the ampersand appears as *it's own token*.
    if ((loc = index(line, '&')) != NULL)
    {
        *background = 1;
        *loc = ' ';
    }
    else
        *background = 0;

    while ((token = strsep(&line, " \t\n")) != NULL)
    {
        token_length = strlen(token);
        for (j = 0; j < token_length; j++)
            if (token[j] <= ' ')
                token[j] = '\0';
        if (strlen(token) > 0)
            args[i++] = token;
    }

    args[i] = NULL;

    return i;
}

void subprocess(char **args)
{
    int r = execvp(args[0], args);
    if(r == -1)
        fprintf(stderr, "error starting subprocess: %s\n", strerror(errno));
    exit(-1);
}

struct JobSpec * dispatch(char **args, int bg, Jobs *jobs, History *history)
{
    int ret = 0, // the return code of the subprocess (set only if !bg)
        is_builtin = 0, // whether we're running a builtin
        pid = 0,  // the pid of the subprocess (not set if !bg and builtin)
        is_forked = 0; // whether we've forked yet (since pid isn't quite enough)

    // the job specification for the job to dispatch
    // Note: dispatch does not set the `command` attribute of the job; the
    // caller gets to do that.
    struct JobSpec *job = create_job(jobs);
    job->bg = bg;

    // if we're in background, then we *must* fork, regardless of whether
    // we're running a builtin.
    if(bg)
    {
        is_forked = 1; // record that we're forking
        pid = fork(); // record the pid, and fork !
    }

    // if we're in the subprocess or we haven't forked yet
    if(pid == 0 || !is_forked)
    {
        // try to run a builtin, and set the builtin flag accordingly
        if(strncmp(args[0], BUILTIN_HISTORY, BUILTIN_HISTORY_LEN) == 0)
        {
            is_builtin = 1;
            ret = builtin_history(history);
        }
        else if(strncmp(args[0], BUILTIN_R, BUILTIN_R_LEN) == 0)
        {
            is_builtin = 1;
            ret = builtin_r(history, jobs, args[1]);
        }
        else if(strncmp(args[0], BUILTIN_JOBS, BUILTIN_JOBS_LEN) == 0)
        {
            is_builtin = 1;
            ret = builtin_jobs(jobs);
        }
        else if(strncmp(args[0], BUILTIN_CD, BUILTIN_CD_LEN) == 0)
        {
            is_builtin = 1;
            ret = builtin_cd(args[1]);
        }
        else if(strncmp(args[0], BUILTIN_PWD, BUILTIN_PWD_LEN) == 0)
        {
            is_builtin = 1;
            ret = builtin_pwd();
        }
        else if(strncmp(args[0], BUILTIN_FG, BUILTIN_FG_LEN) == 0)
        {
            int id = JOBS_MOST_RECENT;
            is_builtin = 1;
            if(args[1] != NULL)
                id = atoi(args[1]); // TODO use strtol
            ret = builtin_fg(jobs, id);
        }
        else if(strncmp(args[0], BUILTIN_EXIT, BUILTIN_EXIT_LEN) == 0)
        {
            is_builtin = 1;
            if(args[1] != NULL)
                ret = atoi(args[1]); // TODO use strtol
            else
                ret = 0;
        }

        // if a builtin did run
        if(is_builtin)
        {
            // and we forked a subprocess
            if(is_forked)
                // exit with the return code of the builtin
                exit(ret);
            else
                // otherwise just record the return code to the JobSpec
                job->return_code = ret;
        }
        else // if a builtin didn't run
        {
            // and if we haven't forked yet, then we must in order to run the
            // subprocess.
            if(!is_forked)
            {
                is_forked = 1;
                pid = fork();
            }

            if(pid == 0)
                // does not return (invokes execvp)
                // or kills the shell if execvp fails
                // TODO gracefully handle execvp failures
                subprocess(args);
        }
    }

    // note: we're in the parent process right now
    // note: pid will be zero if we ran a foreground builtin
    job->pid = pid;

    // if we ran a builtin *in the foreground*, then the job is complete
    if(is_builtin)
        job->running = 0;

    // if we ran a subprocess in the foreground, then we need to wait for it
    if(!bg && pid != 0)
    {
        int pstat;
        waitpid(pid, &pstat, 0);
        job->return_code = WEXITSTATUS(pstat);
    }

    // if the job is run in the background, then we assume it is currently
    // running. Otherwise, we've finished running it, so it's done.
    if(bg)
        job->running = 1;
    else
        job->running = 0;

    // save the return code to the JobSpec
    job->return_code = ret;

    return job;
}

struct JobSpec * evaluate(char *line, History *history, Jobs *jobs)
{
    int bg = 0;
    char *args[20];
    char *history_command = strdup(line);
    char *job_command = strdup(line);

    parse_command(line, args, &bg);

    // if there are no tokens
    if(args[0] == NULL)
        return NULL; // nothing happens :)

    struct JobSpec *job = dispatch(args, bg, jobs, history);

    job->command = job_command;

    if(job->bg && add_job(jobs, job) != 0)
    {
        fprintf(stderr, "jobctl: failed to add job\n");
        return NULL;
    }

    // if history control is enabled and we're not repeating commands from the
    // history.
    if(history != NULL && strcmp(args[0], "r") != 0)
    {
        struct HistoryItem *new_history_item =
            history_create_item(history, history_command);

        history_add_item(history, new_history_item);
    }
    else
        free(history_command);

    return job;
}

int repl(History *h, Jobs *j)
{
    char *line;
    size_t linecap = 0;
    int line_length = 0;
    enum ReplStatus repl_status = REPL_SUCCESS;
    struct JobSpec *last_job = NULL;

    while(repl_status != REPL_EXIT)
    {
        check_jobs(j, JOBS_SHOW);

        // show prompt
        printf("$ ");

        // stdout is line buffered, so we flush the buffer to force writing
        // the prompt to the terminal.
        fflush(stdout);

        // Read a line of stdin check that it's nonempty, and hand it off to
        // evaluate
        line = NULL;
        linecap = 0;
        line_length = getline(&line, &linecap, stdin);

        if (line_length <= 0)
        {
            repl_status = REPL_EXIT;
            continue;
        }

        last_job = evaluate(line, h, j);
        free(line);

        if(last_job == NULL)
            repl_status = REPL_FAILURE;
        else
        {
            if(last_job->bg == 0 && strncmp(last_job->command, "exit", 4) == 0)
                repl_status = REPL_EXIT;
            else
                repl_status = REPL_SUCCESS;
        }
    }

    return 0;
}
