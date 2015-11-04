.PHONY: all

all: bin/fuse_wrappers bin/sfs_test bin/sfs_test2

CFLAGS=-Iinclude -g -D_FILE_OFFSET_BITS=64
LDFLAGS=
CC=gcc

bin/fuse_wrappers: obj/fuse_wrappers.o
	$(CC) $< -o $@ $(LDFLAGS)

bin/sfs_test: obj/sfs_test.o
	$(CC) $< -o $@ $(LDFLAGS)

bin/sfs_test2: obj/sfs_test2.o
	$(CC) $< -o $@ $(LDFLAGS)

obj/fuse_wrappers.o: src/fuse_wrappers.c include/disk_emu.h include/sfs_api.h
	$(CC) $< -c -o $@ $(CFLAGS)
