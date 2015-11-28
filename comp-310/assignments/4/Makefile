.PHONY: all lib test

INCLUDEDIR=include
SRCDIR=src
BINDIR=bin
OBJDIR=obj

$(shell mkdir -p $(BINDIR) $(OBJDIR))

CC=gcc

TESTPROG=malloc_test
CFLAGS=-g -I$(INCLUDEDIR) -Wall -Werror
LDFLAGS=

all: lib test

lib: $(OBJDIR)/jmalloc.o

test: $(BINDIR)/$(TESTPROG)

$(BINDIR)/$(TESTPROG): $(OBJDIR)/malloc_test.o $(OBJDIR)/jmalloc.o
	$(CC) $(CFLAGS) $^ -o $@

$(OBJDIR)/malloc_test.o: $(SRCDIR)/malloc_test.c $(INCLUDEDIR)/jmalloc.h
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJDIR)/jmalloc.o: $(SRCDIR)/jmalloc.c $(INCLUDEDIR)/jmalloc.h
	$(CC) $(CFLAGS) -c $< -o $@
