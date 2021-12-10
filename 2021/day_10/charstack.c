#include <stdlib.h>

typedef struct charstack {
    int size;
    char top;
    struct charstack *prev;
} charstack;

charstack* newStack() { return malloc(sizeof(charstack)); }

char pop(charstack **li) {
    charstack *popped = *li;
    *li = popped->prev;
    char c = popped->top;
    free(popped);
    return c;
}

void push(charstack **li, char c) {
    charstack *deli = *li;
    charstack *head = malloc(sizeof(charstack));
    head->size = deli->size + 1;
    head->top = c;
    head->prev = deli;
    *li = head;
}

