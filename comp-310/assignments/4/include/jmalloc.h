#ifndef JMALLOC_H
#define JMALLOC_H

#include <sys/types.h>

#define JMALLOC_PAGE_SIZE (128 * 1024 * 1024)

#define JMALLOC_STRATEGY_BEST 0
#define JMALLOC_STRATEGY_FIRST 1

/**
 * Represents a number of bytes as requested by a user.
 */
typedef size_t user_size_t;

/**
 * A pointer as given/received to/from a user.
 */
typedef void * user_ptr;

/**
 * Allocates a given amount of space using the current allocation strategy.
 *
 * If successful, returns a pointer to the allocated space.
 * Else, returns NULL.
 */
user_ptr
jmalloc(user_size_t size);

/**
 * Frees previously allocated space. The given pointer must be a value obtained
 * from a call to jmalloc. Else, the result is undefined.
 *
 * If the given pointer is NULL, this function is a no-op.
 */
void
jfree(user_ptr ptr);

/**
 * Changes the current allocation strategy. Valid values are given by the
 * following symbolic constants:
 * * JMALLOC_STRATEGY_BEST_FIT:
 *     Allocates memory within the free block such that the remaining space in
 *     that block is minimized.
 * * JMALLOC_STRATEGY_FIRST_FIT:
 *     Allocates memory within the first found free block having enough
 *     available space.
 */
void
jmallopt(unsigned int strategy);

/**
 * Prints the status of the memory allocator to stdout.
 */
void
print_memory_status();

/**
 * Prints a listing of all memory blocks to stdout.
 */
void
dump_blocks();

/**
 * Some error conditions.
 */
#define JMALLOC_NO_ERROR            ("")
#define INVALID_ALLOCATION_STRATEGY ("invalid allocation strategy")
#define NO_MORE_MEMORY              ("there is no more available memory")
#define INVARIANT_VIOLATED          ("an invariant has been violated")

#endif
