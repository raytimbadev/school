$(shell mkdir -p lib obj)

ARFLAGS = rcs
CFLAGS = -g -Werror -Wall -Iinclude -c

.PHONY: all
all: lib/libprintspool.a

.PHONY: clean
clean:
	@rm -v lib/*
	@rm -v obj/*

lib/libprintspool.a: obj/printspool.o
	$(AR) $(ARFLAGS) $@ $^

obj/printspool.o: src/printspool.c include/printspool.h
	$(CC) $(CFLAGS) -o $@ $<
