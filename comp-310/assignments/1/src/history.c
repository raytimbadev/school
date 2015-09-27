#include "history.h"

#include <stdlib.h>
#include <string.h>

void history_item_destroy(struct HistoryItem *hi)
{
    free(hi->contents);
    free(hi);
}

struct HistoryItem *history_create_item(History *h, char *cmd)
{
    struct HistoryItem *last_history = NULL, *new_history = NULL;

    struct Node *front = h->first;
    if(front != NULL)
        last_history = (struct HistoryItem *)front->data;

    new_history = malloc(sizeof(struct HistoryItem));
    new_history->contents = cmd;

    // if history is empty
    if(last_history == NULL)
        new_history->index = 1;
    else
        new_history->index = last_history->index + 1;

    return new_history;
}

void history_add_item(History *h, struct HistoryItem *hi)
{
    ll_push_front(h, hi);

#if HISTORY_SIZE > 0
    struct Node *back = NULL;
    struct HistoryItem *item = NULL;

    if(h->size > HISTORY_SIZE)
    {
        back = ll_pop_back(h);
        item = (struct HistoryItem *)back->data;
        history_item_destroy(item);
        ll_free_node(back);
    }
#endif
}

struct HistoryItem * history_find_item(History *h, char *cmd)
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

void print_history_sub(struct Node *node)
{
    if(node == NULL)
        return;

    print_history_sub(node->next);

    struct HistoryItem *item = (struct HistoryItem *)node->data;

    printf("%2d: %s", item->index, item->contents);
}

void print_history(History *h)
{
    print_history_sub(h->first);
}
