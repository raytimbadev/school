.PHONY: all

$(shell mkdir -p obj bin)

all: bin/fuse_wrappers bin/sfs_test bin/sfs_test2 bin/sfs_test_jake

CFLAGS=-Iinclude -g -D_FILE_OFFSET_BITS=64 -Wall -Werror
LDFLAGS=
CC=gcc

.PHONY: clean
clean:
	@rm -v obj/* bin/*

bin/fuse_wrappers: obj/fuse_wrappers.o obj/sfs_api.o obj/sfs_helpers.o obj/disk_emu.o
	$(CC) $^ -o $@ -lfuse

bin/sfs_test: obj/sfs_test.o obj/sfs_api.o obj/sfs_helpers.o obj/disk_emu.o
	$(CC) $^ -o $@ $(LDFLAGS)

bin/sfs_test2: obj/sfs_test2.o obj/sfs_api.o obj/sfs_helpers.o obj/disk_emu.o
	$(CC) $^ -o $@ $(LDFLAGS)

bin/sfs_test_jake: obj/sfs_test_jake.o obj/sfs_api.o obj/sfs_helpers.o obj/disk_emu.o
	$(CC) $^ -o $@ $(LDFLAGS)

obj/fuse_wrappers.o: src/fuse_wrappers.c include/disk_emu.h include/sfs_api.h
	$(CC) $< -c -o $@ $(CFLAGS)

obj/sfs_api.o: src/sfs_api.c include/sfs_api.h include/sfs_helpers.h
	$(CC) $< -c -o $@ $(CFLAGS)

obj/sfs_helpers.o: src/sfs_helpers.c include/sfs_api.h include/sfs_helpers.h
	$(CC) $< -c -o $@ $(CFLAGS)

obj/disk_emu.o: src/disk_emu.c include/disk_emu.h
	$(CC) $< -c -o $@ $(CFLAGS)

obj/sfs_test.o: src/sfs_test.c include/sfs_api.h
	$(CC) $< -c -o $@ $(CFLAGS)

obj/sfs_test2.o: src/sfs_test2.c include/sfs_api.h
	$(CC) $< -c -o $@ $(CFLAGS)

obj/sfs_test_jake.o: src/sfs_test_jake.c include/sfs_api.h
	$(CC) $< -c -o $@ $(CFLAGS)
