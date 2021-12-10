#include <stdlib.h>

typedef struct list {
    int size;
    char top;
    struct list *prev;
} list;

list* newStack();
char pop(list **li);
void push(list **li, char c);

