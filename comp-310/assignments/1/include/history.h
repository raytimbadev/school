#ifndef HISTORY_H
#define HISTORY_H

#ifndef HISTORY_SIZE
#define HISTORY_SIZE 10
#endif

#include <stdlib.h>
#include <stdio.h>

#include "ll.h"

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
