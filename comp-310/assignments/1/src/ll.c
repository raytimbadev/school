#include "ll.h"

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

void ll_free_node(struct Node *node)
{
    free(node);
}

void ll_destroy_node(struct Node *node)
{
    free(node->data);
    ll_free_node(node);
}
