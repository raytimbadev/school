#include "jmalloc.h"

#include <unistd.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

/**
 * A typedef for allocation strategies.
 */
typedef struct free_block * (*alloc_strategy)(user_size_t);

struct free_block * find_next_block_first_fit(user_size_t size);
struct free_block * find_next_block_best_fit(user_size_t size);

char buf[256];

/**
 * The current allocation strategy.
 */
static alloc_strategy current_allocation_strategy = find_next_block_best_fit;

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
 * The number of available allocation strategies.
 */
#define JMALLOC_AVAILABLE_STRATEGY_COUNT 2

/**
 * The current program break.
 */
static void * break_limit = NULL;

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
};

/**
 * The list of blocks.
 */
static struct block_list block_list = {
    .first = NULL,
    .last = NULL
};

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
    return break_point;
}

/**
 * Decides whether two blocks can be merged.
 * The first block must come before the second.
 */
char
can_merge(struct free_block *first, struct free_block *second)
{
    if(first == NULL)
        return 0;

    assert(first < second);
    return first + first->size == second;
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
}

/**
 * Merges two blocks.
 * They must a priori have been determined to be mergeable.
 */
void
merge(struct free_block *first, struct free_block *second)
{
    assert(can_merge(first, second));
    first->size += second->size + sizeof(second);
    block_delete(second);
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

    // delete the block from the list, tying its prev to its next and vice
    // versa.
    block_delete(current);

    // compute the remaining number of free bytes after the split.
    const unsigned int remaining = current->size - size;

    // if there isn't enough room to store the block metadata plus at least one
    // byte, then we won't perform a split at all.
    if(remaining < sizeof(struct free_block) + 1)
        return NULL;

    // if there is enough room, then we have to compute where the new block
    // will start (which is immediately after the allocated memory).
    const unsigned int new_offset = size;

    struct free_block *new = current + new_offset;
    *new = (struct free_block) {
        .size = remaining - sizeof(struct free_block),
        .prev = current->prev,
        .next = current->next
    };

    if(current->prev != NULL)
        current->prev->next = new;

    if(current->next != NULL)
        current->next->prev = new;

    sprintf(buf, "allocate_within: splitting 0x%08x at %zu",
            current,
            size);
    puts(buf);

    return new;
}

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

    // no block is available.
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
        block_list.first = new == NULL ? result->next : new;

    if(result == block_list.last)
        block_list.last = new == NULL ? result->next : new;

    // if result had no successor, then we consumed the one and only block, so
    // we have to mark the last pointer as NULL
    if(block_list.first == NULL)
        block_list.last = NULL;

    sprintf(buf, "jmalloc: allocated %zu bytes from block at 0x%08x "
            "(data pointer 0x%08x)",
            size,
            result,
            result->data);
    puts(buf);
    // return the data pointer of the found block
    return result->data;
}

void
jfree(user_ptr ptr)
{
    // traverse the list until we find two free blocks that sandwich the block
    // identified by this pointer
    // mark the block free by tying it back into the free list

    struct free_block *block =
        (struct free_block *)(ptr - offsetof(struct free_block, data));
    struct free_block *prev = NULL;
    struct free_block *current = NULL;

    char found_prev = 0, found_next = 0;

    for(current = block_list.first;
            current != NULL;
            current = current->next,
            found_prev = 0,
            found_next = 0)
    {
        if(prev == NULL || prev < block)
            found_prev = 1;

        if(current == NULL || block < current)
            found_next = 1;

        if(found_next && found_prev)
        {
            block->prev = prev;
            block->next = current;

            sprintf(buf, "jfree: sandwich 0x%08x - 0x%08x for 0x%08x",
                    prev,
                    current,
                    block);
            puts(buf);

            if(current == NULL)
                block_list.last = block;
            else
                current->prev = block;

            if(prev == NULL)
                block_list.first = block;
            else
                prev->next = block;

            break;
        }
    }

    if(current != NULL && can_merge(block, current))
        merge(block, current);

    if(prev != NULL && can_merge(prev, block))
        merge(prev, block);
}

int
free_list_length()
{
    unsigned int i;
    struct free_block *current;

    for(i = 0, current = block_list.first;
            current != NULL;
            i++,
            current = current->next);

    return i;
}
