$(shell mkdir -p bin obj)

CFLAGS = -g -Werror -Wall -Iinclude -I../common/include -c
LDFLAGS = -g -lrt -lpthread

.PHONY: all
all: bin/client

.PHONY: clean
clean:
	@rm -v bin/*
	@rm -v obj/*

bin/client: obj/main.o
	$(CC) -o $@ $(LDFLAGS) $^ ../common/lib/libprintspool.a

obj/main.o: src/main.c
	$(CC) -o $@ $(CFLAGS) $<
