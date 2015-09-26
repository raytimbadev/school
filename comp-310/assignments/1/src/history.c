#include "history.h"

#include <stdlib.h>
#include <string.h>

void ll_push_front(struct LinkedList *ll, void *elem)
{
    struct Node *new = malloc(sizeof(struct Node));

    // Set up the contents of the new node
    // The data stored in the node is given as a parameter
    new->data = elem;

    // New node is on the front, so its next element is the current front of 
    // the list
    new->next = ll->first; 

    // Set the first pointer of the list to the new node.
    ll->first = new;

    // Set the last element to the first element if the list was empty
    if(ll->size == 0)
        ll->last = ll->first;

    ll->size++;
}

void ll_push_back(struct LinkedList *ll, void *elem)
{
    struct Node *new = malloc(sizeof(struct Node));
    new->data = elem;
    new->next = NULL; // since it will be the last element.

    if(ll->last == NULL)
    {
        ll->last = new;
        ll->first = new;
    }
    else
    {
        ll->last->next = new;
        ll->last = new;
    }

    ll->size++;
}

struct Node * ll_pop_front(struct LinkedList *ll)
{
    struct Node *front = ll->first;

    // check for empty list
    if(front == NULL)
        return NULL;

    // check for singleton list
    if(front->next == NULL)
    {
        ll->first = NULL;
        ll->last = NULL;
        return front;
    }

    // otherwise
    ll->first = front->next;
    front->next = NULL;
    return front;
}

struct Node * ll_pop_back(struct LinkedList *ll)
{
    struct Node *prev = NULL;
    struct Node *current = ll->first;

    // empty list
    if(current == NULL)
        return NULL;

    // deal with the special case of the list being empty
    if(ll->size == 1)
    {
        ll->first = NULL;
        ll->last = NULL;
        ll->size = 0;
        return current;
    }

    // seek to the end of the list
    while(current->next != NULL)
    {
        prev = current;
        current = current->next;
    }

    // prev is now the second to last node, i.e. the new last node

    // fix up the last node
    prev->next = NULL;
    ll->last = prev;

    // set new list size
    ll->size--;

    // return the old last node.
    return current;
}

void history_item_destroy(struct HistoryItem *hi)
{
    free(hi->contents);
    free(hi);
}

struct HistoryItem *create_history_item(History *h, char *cmd)
{
    struct HistoryItem *last_history = NULL, *new_history = NULL;
    struct Node *prev = NULL;
    struct Node *current = h->first;

    // seek to the last element of the history
    while(current != NULL)
    {
        prev = current;
        current = current->next;
    }

    new_history = malloc(sizeof(struct HistoryItem));
    new_history->contents = cmd;

    // if history is empty
    if(prev == NULL) 
        new_history->index = 1;
    else
    {
        last_history = (struct HistoryItem *)prev->data;
        new_history->index = last_history->index + 1;
    }

    return new_history;
}

void add_history_item(History *h, struct HistoryItem *hi)
{
    struct HistoryItem *front = NULL;

    ll_push_back(h, hi);

#if HISTORY_SIZE > 0
    if(h->size > HISTORY_SIZE)
    {
        front = (struct HistoryItem *)ll_pop_front(h);
        history_item_destroy(front);
    }
#endif
}

struct HistoryItem * history_find(History *h, char *cmd)
{
    size_t good_len = strcspn(cmd, "\n");
    struct Node *current = h->first;
    struct HistoryItem *current_item = NULL;

    while(current != NULL)
    {
        current_item = (struct HistoryItem *)current->data;
        if(strncmp(current_item->contents, cmd, good_len) == 0)
            return current_item;
        current = current->next;
    }

    return NULL;
}

void print_history(History *h)
{
    struct Node *current = h->first;
    struct HistoryItem *item = NULL;

    for(; current != NULL; current = current->next)
    {
        item = (struct HistoryItem *)current->data;
        printf("%2d: %s", item->index, item->contents);
    }
}
