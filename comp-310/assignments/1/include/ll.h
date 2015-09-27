#ifndef LL_H
#define LL_H

#include <stdlib.h>

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

/**
 * Frees the memory of a Node structure.
 * The data held in the ndoe is not freed.
 */
void ll_free_node(struct Node *node);

/**
 * Frees the memory of a Node structure, and the data held by the node.
 */
void ll_destroy_node(struct Node *node);

#endif
