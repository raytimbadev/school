#include "repl.h"

int main()
{
    History *history  = malloc(sizeof(History));
    Jobs *jobs = malloc(sizeof(Jobs));

    int ret = repl(history, jobs);

    // TODO cleanup logic for history and jobs

    return ret;
}
