.PHONY: all

CC = gcc
LDFLAGS = 
CFLAGS = -c -Og -Wall -Werror

all: shell

shell: main.o
	$(CC) $(LDFLAGS) -o $@ $<

main.o: main.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	@rm -v *.o shell
