.PHONY: all
all: server client

AR = ar
CC = gcc

.PHONY: server
server: common
	make -C server

.PHONY: client
client: common
	make -C client

.PHONY: common
common:
	make -C common

.PHONY: clean
clean:
	-make -C client clean
	-make -C server clean
	-make -C common clean
