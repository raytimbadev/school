#include "jmalloc.h"

#include <unistd.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

/**
 * A buffer used internally to print strings.
 */
static char buf[256];

/**
 * Allocation errors are stored here.
 */
char *jmalloc_error = JMALLOC_NO_ERROR;

/**
 * The start of the data segment. Initialized on the first call to either
 * jmalloc or jfree.
 */
static void * break_start = NULL;

/**
 * A free block as a node in a doubly-linked list.
 */
struct free_block
{
    user_size_t size;
    struct free_block *prev;
    struct free_block *next;
    char data[0];
};

struct block_list
{
    struct free_block *first;
    struct free_block *last;
    size_t alloc_total;
    size_t free_total;
};

/**
 * The list of blocks.
 */
static struct block_list block_list = {
    .first = NULL,
    .last = NULL,
    .alloc_total = 0,
    .free_total = 0
};

/**
 * A free block searching strategy.
 *
 * Finds the first available block having at least the given amount of free
 * space.
 */
struct free_block *
find_next_block_first_fit(user_size_t size)
{
    struct free_block *current = NULL;

    for(current = block_list.first;
            current != NULL;
            current = current->next)
        if(current->size >= size)
            // We found a winner !
            break;

    // if no block is available, then current will be NULL.
    return current;
}

/**
 * A free space searching strategy.
 *
 * Searches the free space list for a free block with size at least a given
 * amount and for which the difference between its available space and the
 * given size is minimal.
 */
struct free_block *
find_next_block_best_fit(user_size_t size)
{
    struct free_block *current = NULL;

    struct free_block *best = NULL;

    for(current = block_list.first;
            current != NULL;
            current = current->next)
        if(current->size >= size &&
                (best == NULL || current->size < best->size))
            best = current;

    return best;
}

/**
 * A typedef for allocation strategies.
 */
typedef struct free_block * (*alloc_strategy)(user_size_t);

/**
 * The available allocation strategies, mapped to the symbolic constants
 * * JMALLOC_STRATEGY_BEST
 * * JMALLOC_STRATEGY_FIRST
 */
static alloc_strategy allocation_strategies[2] = {
    find_next_block_best_fit,
    find_next_block_first_fit
};

/**
 * The current allocation strategy.
 */
static alloc_strategy current_allocation_strategy = find_next_block_best_fit;

/**
 * The number of available allocation strategies.
 */
#define JMALLOC_AVAILABLE_STRATEGY_COUNT 2

/**
 * Bumps up the breakpoint and allocates a new block in the newly allocated
 * space.
 *
 * The newly allocated block has its next and prev pointers set to NULL!
 */
struct free_block *
break_up() // :'(
{
    struct free_block *break_point
        = (struct free_block *)sbrk(JMALLOC_PAGE_SIZE);
    *break_point = (struct free_block) {
        .next = NULL,
        .prev = NULL,
        .size = JMALLOC_PAGE_SIZE - sizeof(*break_point)
    };

    block_list.free_total += break_point->size - sizeof(*break_point);
    return break_point;
}

/**
 * Brings down the program break. As a precondition, it is required that the
 * last block of the free list have size greater than JMALLOC_PAGE_SIZE.
 *
 * Returns 0 on success and -1 on failure.
 */
int
break_down()
{
    assert(block_list.last->size > JMALLOC_PAGE_SIZE);

    int ret = brk(
            (void*)block_list.last +
            block_list.last->size -
            JMALLOC_PAGE_SIZE);
    if(ret == 0)
        block_list.last->size -= JMALLOC_PAGE_SIZE;

    block_list.free_total -= JMALLOC_PAGE_SIZE;
    return ret;
}

/**
 * Deletes a block from a doubly linked list.
 *
 * After calling this function, traversing the list will not find the given
 * block.
 */
void
block_delete(struct free_block *b)
{
    if(b->prev != NULL)
        b->prev->next = b->next;

    if(b->next != NULL)
        b->next->prev = b->prev;

    if(b == block_list.last)
        block_list.last = b->prev;

    if(b == block_list.first)
        block_list.first = b->next;
}

/**
 * Decides whether two blocks can be merged.
 * The first block must come before the second.
 *
 * Either block may be NULL, in which case the result is simply `false`.
 */
char
can_merge(struct free_block *first, struct free_block *second)
{
    if(first == NULL || second == NULL)
        return 0;

    assert(first < second);
    return (void*)first + sizeof(struct free_block) + first->size
        == (void*)second;
}

/**
 * Merges two blocks.
 * They must a priori have been determined to be mergeable.
 */
void
merge(struct free_block *first, struct free_block *second)
{
    assert(can_merge(first, second));

    first->size += second->size + sizeof(*second);
    block_delete(second);

    if(block_list.last == second)
        block_list.last = first;

    // the space that used to be used by the split metadata may now be used
    // for storage.
    block_list.free_total += sizeof(*second);
}

/**
 * Splits the current block to allocate a certain amount of space.
 *
 * If after splitting, the amount of free space is too small to accommodate the
 * metadata associated with a free block, then no split is performed and the
 * free block is completely consumed.
 *
 * The given current block is removed from the linked list and a new block is
 * created if the split takes place. The newly created block is the return
 * value. If no split occurs, then the return value is NULL.
 *
 * An assumed precondition is: size <= current->size
 */
struct free_block *
allocate_within(struct free_block *current, user_size_t size)
{
    assert(size <= current->size);

    // compute the remaining number of free bytes after the split.
    const unsigned int remaining = current->size - size;

    // if there isn't enough room to store the block metadata plus at least one
    // byte, then we won't perform a split at all.
    if(remaining < sizeof(struct free_block) + 1)
    {
        sprintf(buf, "allocate_within: insufficient space (%zu) in block "
                "for split to allocate %zu.",
                current->size,
                size);
        puts(buf);
        block_delete(current);
        return NULL;
    }

    // if there is enough room, then we have to compute where the new block
    // will start (which is immediately after the allocated memory).
    const unsigned int new_offset = size + sizeof(struct free_block);

    struct free_block *new = (void*)current + new_offset;
    *new = (struct free_block) {
        .size = remaining - sizeof(struct free_block),
        .prev = current->prev,
        .next = current->next
    };

    // make sure to adjust the ties this block has coming into it so they point
    // to the new block start.
    if(current->prev != NULL)
        current->prev->next = new;

    if(current->next != NULL)
        current->next->prev = new;

    sprintf(buf, "allocate_within: splitting %p at %zu (%p)",
            (void*)current,
            size,
            (void*)new);
    puts(buf);

    // the space used to store the block metadata may no longer be used for
    // storage.
    block_list.free_total -= sizeof(struct free_block);

    return new;
}

void
block_append(struct free_block *block)
{
    if(block_list.first == NULL)
    {
        block_list.first = block;
        block_list.last = block;
        block->prev = NULL;
        block->next = NULL;
    }
    else
    {
        block->prev = block_list.last;
        block_list.last->next = block;
        block_list.last = block;
    }
}

user_ptr
jmalloc(user_size_t size)
{
    if(break_start == NULL)
        break_start = sbrk(0);

    struct free_block * result = current_allocation_strategy(size);

    // loop until we have a big enough free block
    while(result == NULL || result->size < size)
    {
        struct free_block *block = break_up();
        block_append(block);
        if(block_list.last != block && can_merge(block_list.last, block))
        {
            merge(block_list.last, block);
            result = block_list.last;
        }
        else
            result = block;
    }

    // perform the block split.
    struct free_block *new = allocate_within(result, size);

    // make sure we push back the first pointer
    if(result == block_list.first)
    {
        block_list.first = new == NULL ? result->next : new;

        if(block_list.first->next != NULL)
            block_list.first->next->prev = block_list.first;
    }

    // make sure we push 
    if(result == block_list.last)
    {
        block_list.last = new == NULL ? result->next : new;
        
        if(block_list.last->prev != NULL)
            block_list.last->prev->next = block_list.last;
    }

    // if result had no successor, then we consumed the one and only block, so
    // we have to mark the last pointer as NULL
    if(block_list.first == NULL)
        block_list.last = NULL;

    sprintf(buf, "jmalloc: allocated %zu bytes from block at %p "
            "(data pointer %p)",
            size,
            (void*)result,
            (void*)result->data);
    puts(buf);
    // adjust the known size of the allocated block
    result->size = size;

    // and erase its pointers in the linked list
    result->next = NULL;
    result->prev = NULL;

    // adjust the counts of free and used memory
    block_list.free_total -= result->size;
    block_list.alloc_total += result->size;

    // return the data pointer of the found block
    return result->data;
}

void
jfree(user_ptr ptr)
{
    if(break_start == NULL)
        break_start = sbrk(0);

    // jfree is a noop if the given pointer is NULL.
    if(ptr == NULL)
        return;

    // traverse the list until we find two free blocks that sandwich the block
    // identified by this pointer
    // mark the block free by tying it back into the free list

    struct free_block *block =
        (struct free_block *)((void*)ptr - offsetof(struct free_block, data));

    sprintf(buf, "jfree: freeing %p.", (void*)block);
    puts(buf);

    struct free_block *prev = NULL;
    struct free_block *current = NULL;

    char freed = 0;

    for(current = block_list.first;
            current != NULL || prev != NULL;
            prev = current,
            current = current == NULL ? NULL : current->next) 
    {
        if((current != NULL && block >= current)
                || (prev != NULL && prev >= block))
            continue;

        block->prev = prev;
        block->next = current;

        sprintf(buf, "jfree: sandwich %p - %p for %p",
                (void*)prev,
                (void*)current,
                (void*)block);
        puts(buf);

        if(current == NULL)
            block_list.last = block;
        else
            current->prev = block;

        if(prev == NULL)
            block_list.first = block;
        else
            prev->next = block;

        freed = 1;
        break;
    }

    if(!freed)
    {
        sprintf(buf, "jfree: failed to free %p", (void*)block);
        puts(buf);
    }

    block_list.free_total += block->size;
    block_list.alloc_total -= block->size;

    if(current != NULL && can_merge(block, current))
        merge(block, current);

    if(prev != NULL && can_merge(prev, block))
        merge(prev, block);

    // lower the program break if the top block is very large
    if(block_list.last->size > JMALLOC_PAGE_SIZE)
        break_down();
}

void jmallopt(unsigned int strategy)
{
    if(strategy < 2)
    {
        jmalloc_error = INVALID_ALLOCATION_STRATEGY;
        return;
    }

    current_allocation_strategy = allocation_strategies[strategy];
}

void print_memory_status()
{
    if(break_start == NULL)
        break_start = sbrk(0);

    const size_t total = (size_t)sbrk(0) - (size_t)break_start;

    puts("--- MEMORY STATUS ---");
    sprintf(buf, "free: %zu\nused: %zu\ntotal: %zu\noverhead: %zu",
            block_list.free_total,
            block_list.alloc_total,
            total,
            total - block_list.free_total - block_list.alloc_total);
    puts(buf);
}

void dump_blocks()
{
    if(break_start == NULL)
        break_start = sbrk(0);

    struct free_block *current;
    struct free_block *prev;

    puts("== dumping blocks");

    for(prev = NULL,
            current = block_list.first;
            current != NULL || prev != NULL;
            prev = current,
            current = current == NULL ? NULL : current->next)
    {
        if(prev == NULL && current != break_start)
        {
            // first block is allocated
            sprintf(buf, "%p - %p (%zu): USED",
                    (void*)break_start,
                    (void*)current,
                    (size_t)((void*)current - (void*)break_start));
            puts(buf);
            continue;
        }

        if(prev == NULL)
            continue;

        void *begin = (void*)prev + prev->size + sizeof(struct free_block);

        sprintf(buf, "%p - %p (%zu): FREE",
                (void*)prev,
                begin,
                prev->size);
        puts(buf);

        void *end;

        if(current == NULL)
            end = sbrk(0);
        else
            end = current;

        if(begin == end)
            continue;

        sprintf(buf, "%p - %p (%zu): USED",
                begin,
                (void*)current,
                (size_t)((void*)current - begin));
        puts(buf);
    }

    puts("== dumping finished");
}
