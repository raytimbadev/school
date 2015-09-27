#include "history.h"

#include <stdlib.h>
#include <string.h>

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
    struct Node *front = NULL;
    struct HistoryItem *item = NULL;

    ll_push_back(h, hi);

#if HISTORY_SIZE > 0
    if(h->size > HISTORY_SIZE)
    {
        front = ll_pop_front(h);
        item = (struct HistoryItem *)front->data;
        history_item_destroy(item);
        ll_free_node(front);
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
