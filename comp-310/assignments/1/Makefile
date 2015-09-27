.PHONY: all

CC = gcc
LDFLAGS = 
CFLAGS = -c -g -Og -Wall -Werror -Iinclude

_ = $(shell mkdir -p obj bin src)

all: bin/shell

bin/shell: obj/main.o obj/history.o obj/builtins.o obj/ll.o obj/jobs.o obj/repl.o
	$(CC) $(LDFLAGS) -o $@ $^

obj/main.o: src/main.c
	$(CC) $(CFLAGS) -o $@ $<

obj/history.o: src/history.c include/history.h
	$(CC) $(CFLAGS) -o $@ $<

obj/builtins.o: src/builtins.c include/builtins.h include/history.h
	$(CC) $(CFLAGS) -o $@ $<

obj/ll.o: src/ll.c include/ll.h
	$(CC) $(CFLAGS) -o $@ $<

obj/jobs.o: src/jobs.c include/jobs.h
	$(CC) $(CFLAGS) -o $@ $<

obj/repl.o: src/repl.c include/repl.h include/history.h include/jobs.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	@rm -v obj/*.o bin/shell
