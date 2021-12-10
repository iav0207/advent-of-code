#include <stdlib.h>

typedef struct charstack {
    int size;
    char top;
    struct charstack *prev;
} charstack;

charstack* newStack();
char pop(charstack **li);
void push(charstack **li, char c);

