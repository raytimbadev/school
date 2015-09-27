#ifndef BUILTINS_H
#define BUILTINS_H

#define BUILTIN_HISTORY     "history"
#define BUILTIN_HISTORY_LEN 7
#define BUILTIN_R           "r"
#define BUILTIN_R_LEN       1
#define BUILTIN_JOBS        "jobs"
#define BUILTIN_JOBS_LEN    4
#define BUILTIN_CD          "cd"
#define BUILTIN_CD_LEN      2
#define BUILTIN_PWD         "pwd"
#define BUILTIN_PWD_LEN     3
#define BUILTIN_FG          "fg"
#define BUILTIN_FG_LEN      2
#define BUILTIN_EXIT        "exit"
#define BUILTIN_EXIT_LEN    4

#include "jobs.h"
#include "history.h"

int builtin_history(History *h);
int builtin_r(History *h, Jobs *j, char *prefix);
int builtin_cd(char *dst);
int builtin_pwd();
int builtin_jobs(Jobs *jobs);
int builtin_fg(Jobs *jobs, int id);

#endif
