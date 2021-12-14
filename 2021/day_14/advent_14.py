#!/usr/bin/python3

import sys
import json
from collections import defaultdict, deque

steps = int(sys.argv[1] if len(sys.argv) > 1 else 40)
lines = [line for line in sys.stdin.read().split('\n') if line]

template = lines[0]
pairs = {k: v for k, v in [pair.split(" -> ") for pair in lines[1:]]}

debug = False


def log(something):
    if not debug:
        return
    if isinstance(something, str) or isinstance(something, int):
        print(something)
    else:
        print(json.dumps(something, indent=2))


log(f'template: {template}')
log(pairs)

pair_count = defaultdict(int)


def add(pair, diff):
    pair_count[pair] += diff


for i in range(len(template) - 1):
    pair = template[i:i+2]
    add(pair, 1)


todo = deque()

for _ in range(steps):
    for pair, char in pairs.items():
        occurrences = pair_count[pair]
        if occurrences > 0:
            todo.append((pair[0]+char, occurrences))
            todo.append((char+pair[1], occurrences))
            todo.append((pair,        -occurrences))
    while todo:
        add(*todo.popleft())


char_count = defaultdict(int)

items = list(pair_count.items())

log(items)

char_count[template[0]] += 1
for pair, occurrences in items:
    char_count[pair[1]] += occurrences

log(char_count)

min_char, min_count = min(char_count.items(), key=lambda it: it[1])
max_char, max_count = max(char_count.items(), key=lambda it: it[1])

print(f'After {steps} steps:')
print(f'Most frequent character is {min_char}: {min_count} occurrences')
print(f'Least frequent character is {max_char}: {max_count} occurrences')
print(f'Diff: {max_count - min_count}')

