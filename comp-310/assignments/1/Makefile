.PHONY: all

CC = gcc
LDFLAGS = 
CFLAGS = -c -g -Og -Wall -Werror -Iinclude

_ = $(shell mkdir -p obj bin src)

all: bin/shell

bin/shell: obj/main.o obj/history.o
	$(CC) $(LDFLAGS) -o $@ $^

obj/main.o: src/main.c
	$(CC) $(CFLAGS) -o $@ $<

obj/history.o: src/history.c include/history.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	@rm -v obj/*.o bin/shell
