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
	$(CC) $(LDFLAGS) -o $@ $^ ../common/lib/libprintspool.a

obj/main.o: src/main.c
	$(CC) $(CFLAGS) -o $@ $<
