#ifndef REPL_H
#define REPL_H

#include "history.h"
#include "jobs.h"
#include "builtins.h"

enum ReplStatus
{
    REPL_SUCCESS,
    REPL_FAILURE,
    REPL_EXIT
};

struct JobSpec * evaluate(char *line, History *history, Jobs *jobs);

int repl(History *history, Jobs *jobs);



#endif
