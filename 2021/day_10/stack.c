#include <stdlib.h>

typedef struct list {
    int size;
    char top;
    struct list *prev;
} list;

list* newStack() { return malloc(sizeof(list)); }

char pop(list **li) {
    list *popped = *li;
    *li = popped->prev;
    char c = popped->top;
    free(popped);
    return c;
}

void push(list **li, char c) {
    list *deli = *li;
    list *head = malloc(sizeof(list));
    head->size = deli->size + 1;
    head->top = c;
    head->prev = deli;
    *li = head;
}

