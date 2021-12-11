#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "charstack.h"

char *brackets = "([{<)]}>";
int indexOf(char bracket) { return (int) (strchr(brackets, bracket) - brackets); }
bool isOpening(char bracket) { return indexOf(bracket) < 4; }
bool isClosing(char bracket) { return !isOpening(bracket); }
char pair(char bracket) { return brackets[(indexOf(bracket) + 4) % strlen(brackets)]; }

int errpts[4] = { 3, 57, 1197, 25137 };
int errPoints(char closingBracket) { return errpts[indexOf(closingBracket) - 4]; }

char *crank = " )]}>";
int completionRank(char closingBracket) { return (int) (strchr(crank, closingBracket) - crank); }

int compare(const void * a, const void * b) {
    long cmp = *(long*)a - *(long*)b;
    if (cmp > 0) return 1;
    if (cmp < 0) return -1;
    return 0;
}
void sort(long* arr, long size) { qsort(arr, size, sizeof(long), compare); }

int corrupted = 0;
int errorScore = 0;

int incomplete = 0;
long completionScores[200];
long midCompletionScore() {
    sort(completionScores, incomplete);
    return completionScores[incomplete / 2];
}

char* readLine() {
    char* line = malloc(120);
    char c;
    int i = 0;
    while (scanf("%c", &c) == 1 && c != '\n') line[i++] = c;
    return line;
}

void process(char* line) {
    charstack *stack = newStack();
    bool valid = true;
    int i = 0;
    char c;

    while ((c = line[i++]) != '\0') {
        if (isOpening(c)) {
            push(&stack, c);
            continue;
        }
        // c is a closing bracket
        valid = pop(&stack) == pair(c);
        if (!valid) {
            corrupted++;
            errorScore += errPoints(c);
            break;
        }
    }

    // all the not corrupted lines are incomplete (by the problem statement)
    bool complete = c != '\0';
    if (complete) return;

    while (stack->size) {
        completionScores[incomplete] *= 5;
        completionScores[incomplete] += completionRank(pair(pop(&stack)));
    }
    incomplete++;
}

int main() {
    char *line;
    while (strlen(line = readLine())) { process(line); }
    printf("Processed %d corrupted and %d incomplete lines.\n", corrupted, incomplete);
    printf("Syntax error score: %d\n", errorScore);
    printf("Completion score: %ld\n", midCompletionScore());
    return 0;
}

