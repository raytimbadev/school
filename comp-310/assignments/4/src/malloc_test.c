#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "jmalloc.h"

#define BIG_ALLOC_COUNT  10
#define BIG_ALLOC_FACTOR (1024 * 1024)

const unsigned int increments[5] = {
    4,
    8,
    12,
    5,
    9
};

/**
 * Tests a small allocation followed immediately by a free.
 */
void test_alloc_free()
{
    void *ptr;

    unsigned int i = 0;
    for(i = 0; i < BIG_ALLOC_COUNT; i ++)
    {
        ptr = jmalloc(increments[i % 5]);
        dump_blocks();
        jfree(ptr);
        dump_blocks();
    }
}

/**
 * Allocates a bunch of large chunks of memory, and then frees them in the same
 * order they were allocated.
 */
void test_big_allocs()
{
    void *ptrs[BIG_ALLOC_COUNT];

    dump_blocks();

    unsigned int i = 0;
    for(i = 0; i < BIG_ALLOC_COUNT; i++)
        ptrs[i] = jmalloc(BIG_ALLOC_FACTOR * increments[i % 5]);

    dump_blocks();

    for(i = 0; i < BIG_ALLOC_COUNT; i++)
        jfree(ptrs[i]);

    dump_blocks();
}

/**
 * Allocates a bunch of large chunks of memory, and then frees them in reverse
 * allocation order.
 */
void test_big_allocs_reverse()
{
    void *ptrs[BIG_ALLOC_COUNT];

    dump_blocks();

    int i = 0;
    for(i = 0; i < BIG_ALLOC_COUNT; i++)
        ptrs[i] = jmalloc(BIG_ALLOC_FACTOR * increments[i % 5]);

    dump_blocks();

    while(i --> 0)
        jfree(ptrs[i]);

    dump_blocks();
}

void test_interleave()
{
    void *ptrs[BIG_ALLOC_COUNT];

    unsigned int i = 0;
    for(i = 0; i < BIG_ALLOC_COUNT; i++)
    {
        ptrs[i] = jmalloc(increments[i % 5]);

        dump_blocks();

        if(i > 4)
        {
            jfree(ptrs[i - 5]);
            assert(ptrs[i - 5] != NULL);
            ptrs[i - 5] = NULL;
            dump_blocks();
        }
    }

    unsigned int j;
    for(j = i - 4; j < i; j++)
    {
        assert(ptrs[j] != NULL);
        jfree(ptrs[j]);
        ptrs[j] = NULL;
        dump_blocks();
    }
}

void
run_all_tests(int allocation_strategy)
{
    puts("========== Running all tests ==========");

    jmallopt(allocation_strategy);
    print_memory_status();

    puts("===== testing alloc free");
    test_alloc_free();
    puts("===== DONE ");

    print_memory_status();

    puts("===== testing big allocs");
    test_big_allocs();
    puts("===== DONE ");

    print_memory_status();

    puts("===== testing big allocs reversed");
    test_big_allocs_reverse();
    puts("===== DONE ");

    print_memory_status();

    puts("===== testing interleaving");
    test_interleave();
    puts("===== DONE ");

    print_memory_status();

    puts("========== DONE ==========");
}

int main(int argc, char **argv)
{
    run_all_tests(JMALLOC_STRATEGY_BEST);
    run_all_tests(JMALLOC_STRATEGY_FIRST);
    return 0;
}
