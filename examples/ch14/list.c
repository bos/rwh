#include <stdio.h>

/** snippet list */
struct node {
    struct node *next;
};

struct list {
    struct node *head, *tail;
};
    
void list_append(struct list *list, struct list *new_tail)
{
    list->tail->next = new_tail->head;
    list->tail = new_tail->tail;

    /* new_tail is no longer valid */

    new_tail->head = NULL;
    new_tail->tail = NULL;
}
/** /snippet list */
