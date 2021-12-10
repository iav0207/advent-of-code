#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "stack.h"

void stop() { printf("Stopped.\n"); exit(0); }
void assert(bool condition, char *msg) { if (!condition) { printf("%s\n", msg); exit(1); } }

char *brackets = "([{<)]}>";
int indexOf(char c) { return (int) (strchr(brackets, c) - brackets); }
bool isOpening(char c) { return indexOf(c) < 4; }
bool isClosing(char c) { return !isOpening(c); }
char pair(char c) { return brackets[(indexOf(c) + 4) % strlen(brackets)]; }

int errpts[4] = { 3, 57, 1197, 25137 };
int errPoints(char c) { return errpts[indexOf(c) - 4]; }

char *crank = " )]}>";
int completionRank(char c) { return (int) (strchr(crank, c) - crank); }

int corrupted = 0;
int incomplete = 0;
int stxErrScore = 0;
int completionScore = 0;

char* readLine() {
    char* line = malloc(1200);
    char c;
    int i = 0;
    while (scanf("%c", &c) == 1 && c != '\n') line[i++] = c;
    return line;
}

void process(char* line) {
    list *stack = newStack();
    char c;
    bool valid = true;
    int i = 0;
    while ((c = line[i++]) != '\0') {
        if (isOpening(c)) {
            push(&stack, c);
            continue;
        }
        // c is a closing bracket
        valid = pop(&stack) == pair(c);
        if (!valid) {
            corrupted++;
            int points = errPoints(c);
            stxErrScore += points;
            break;
        }
    }
}

int main() {
    char *line;
    while (strlen(line = readLine()) > 0) process(line);
    printf("Processed %d corrupted and %d incomplete lines.\n", corrupted, incomplete);
    printf("Syntax error score: %d\n", stxErrScore);
    printf("Completion score: %d\n", completionScore);
    return 0;
}

