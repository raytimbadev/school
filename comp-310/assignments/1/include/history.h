#ifndef HISTORY_H
#define HISTORY_H

#ifndef HISTORY_SIZE
#define HISTORY_SIZE 10
#endif

#include <stdlib.h>
#include <stdio.h>

struct Node
{
    void * data;
    struct Node *next;
};

struct LinkedList
{
    struct Node *first;
    struct Node *last;
    int size;
};

/**
 * Creates a new linked list with no contents.
 */
struct LinkedList * ll_create_empty();

/**
 * Creates a new linked list with one element.
 */
struct LinkedList * ll_singleton(void * elem);

/**
 * Creates a new inked list from an array.
 */
struct LinkedList * ll_from_array(void *array, size_t elem_size, size_t elem_count);

/**
 * Creates a new array from a linked list.
 */
void * ll_to_array(struct LinkedList * ll, size_t elem_size);

/**
 * Pushes one element onto the front of a linked list.
 */
void ll_push_front(struct LinkedList * ll, void * elem);

/**
 * Pushes one element onto the back of a linked list.
 */
void ll_push_back(struct LinkedList * ll, void * elem);

/**
 * Removes one element from the front of a linked list.
 * The returned pointer is NULL if the list is empty.
 * The returned Node must be freed by the caller.
 */
struct Node * ll_pop_front(struct LinkedList * ll);

/**
 * Removes one element from the back of a linked list.
 * The returned pointer is NULL if the list is empty.
 * The returned Node must be freed by the caller.
 */
struct Node * ll_pop_back(struct LinkedList * ll);

/**
 * Gets the first value of a linked list.
 * The returned pointer is NULL if the list is empty.
 * If non-null, the returned Node must be freed by the caller.
 */
struct Node * ll_peek_front(struct LinkedList * ll);

/**
 * Gets the last value of a linked list.
 * The returned Node is NULL if the list is empty.
 * If non-null, the returned Node must be freed by the caller.
 */
struct Node * ll_peek_back(struct LinkedList * ll);

/**
 * Duplicates a linked list.
 */
struct LinkedList ll_dup(struct LinkedList * ll);

/**
 * Concatenates two linked lists into a new one.
 */
struct LinkedList ll_concat(struct LinkedList *ll_a, struct LinkedList *ll_b);

/**
 * Appends each element of linked list B onto linked list A, in turn.
 */
void ll_extend(struct LinkedList *ll_a, struct LinkedList *ll_b);

/**
 * Creates a linked list viewing a sublist of another linked list.
 */
void ll_sublist(struct LinkedList *ll, int start, int length);

/**
 * Fetches an element as a specific index from a listed list.
 */
void * ll_at(struct LinkedList *ll, int index);

/**
 * Frees the memory of the list structure.
 * The data held in the list is not freed.
 */
void ll_free(struct LinkedList *ll);

/**
 * Frees the memory of the list structure, and the data held by the list.
 */
void ll_destroy(struct LinkedList *ll);

typedef struct LinkedList History;

struct HistoryItem
{
    int index;
    char * contents;
};

/**
 * Frees all memory used by a history item, i.e. its string contents and the
 * HistoryItem structure.
 */
void history_item_destroy(struct HistoryItem *hi);

/**
 * Make some history.
 *
 * Adds a 
 */
void add_history_item(History *h, struct HistoryItem *hi);

/**
 * Creates a new HistoryItem with the correct history index within a given
 * History and returns it.
 * The HistoryItem becomes the owner of the string, and functions involving
 * HistoryItems may call free on it. Pass a copy if this is undesirable.
 * Note that the HistoryItem is not automatically added to the History.
 */
struct HistoryItem * create_history_item(History *h, char *cmd);

/**
 * Finds a HistoryItem in a History such that the provided string is a prefix
 * of the HistoryItem's string.
 */
struct HistoryItem * find_history_item(History *h, char *cmd);

/**
 * Print the command history.
 */
void print_history(History *h);

#endif
