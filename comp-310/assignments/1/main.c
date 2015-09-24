#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>


int getcmd(char *prompt, char *args[], int *background)
{
    int length, i = 0;
    char *token, *loc;
    char *line;
    size_t linecap = 0;

    printf("%s", prompt);
    fflush(stdout);
    length = getline(&line, &linecap, stdin);

    if (length <= 0) {
        fprintf(stderr, "error reading user input\n");
        exit(-1);
    }

    // Check if background is specified..
    // TODO check that the ampersand appears as *it's own token*.
    if ((loc = index(line, '&')) != NULL) {
        *background = 1;
        *loc = ' ';
    } else
        *background = 0;

    while ((token = strsep(&line, " \t\n")) != NULL) {
        int j;
        for (j = 0; j < strlen(token); j++)
            if (token[j] <= 32)
                token[j] = '\0';
        if (strlen(token) > 0)
            args[i++] = token;
    }

    return i;
}


int main()
{
    char *args[20];
    int bg;
    
    while(1) 
    {
        getcmd(">>  ", args, &bg);

        //for (int i = 0; i < cnt; i++)
        //    printf("\nArg[%d] = %s", i, args[i]);

        //if (bg)
        //    printf("\nBackground enabled..\n");
        //else
        //    printf("\nBackground not enabled \n");

        int pid = fork();

        if(pid == 0)
        {
            //printf("hello from the other side!\n");
            int r = execvp(args[0], &args[1]);
            if(r == -1)
            {
                fprintf(stderr, "error invoking process: %s\n", strerror(errno));
            }
            exit(-1);
        }

        waitpid(pid, NULL, 0);

        //printf("\n\n");
    }
}
