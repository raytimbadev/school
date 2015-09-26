#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>

#include "history.h"

#define REPL_SUCCESS 0
#define REPL_FAILURE 1

History *HISTORY = NULL;

int parse_command(char *line, char *args[], int *background)
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
        fprintf(stderr, "error invoking process: %s\n", strerror(errno));
    exit(-1);
}

int evaluate(char *line)
{
    int bg = 0;
    char *args[20];
    char *orig_line = strdup(line);
    struct HistoryItem *new_history_item = NULL;

    int arg_count = parse_command(line, args, &bg);

    if(arg_count == 0)
        return REPL_SUCCESS;

    int pid = fork();

    if(pid == 0)
        subprocess(args);

    if(!bg)
        waitpid(pid, NULL, 0);

    new_history_item = create_history_item(HISTORY, orig_line);
    add_history_item(HISTORY, new_history_item); 

    return REPL_SUCCESS;
}

int main()
{
    char *line;
    size_t linecap = 0;
    int line_length = 0;

    HISTORY = malloc(sizeof(History));
    
    while(1) 
    {
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
            fprintf(stderr, "error reading user input\n");
            continue;
        }

        evaluate(line);

        print_history(HISTORY);
    }
}
