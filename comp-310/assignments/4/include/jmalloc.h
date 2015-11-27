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

user_ptr
jmalloc(user_size_t size);

void
jfree(user_ptr ptr);

#endif
