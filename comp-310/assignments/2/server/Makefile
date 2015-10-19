$(shell mkdir -p bin obj)

CFLAGS = -g -Werror -Wall -Iinclude -I ../common/include -c
LDFLAGS = -g -lrt -lpthread

.PHONY: all
all: bin/server

.PHONY: clean
clean:
	@-rm -vf bin/*
	@-rm -vf obj/*

bin/server: obj/main.o
	$(CC) -o $@ $^ ../common/lib/libprintspool.a $(LDFLAGS)

obj/main.o: src/main.c
	$(CC) $(CFLAGS) -o $@ $<
