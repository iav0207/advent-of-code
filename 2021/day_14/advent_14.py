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

index = defaultdict(int)
for i in range(len(template) - 1):
    pair = template[i] + template[i+1]#template[i:i+1]
    index[pair] += 1


def apply(pair, diff):
    index[pair] += diff


queue = deque()

for i in range(steps):
    for pair, char in pairs.items():
        if index[pair] > 0:
            queue.append((pair[0]+char, index[pair]))
            queue.append((char+pair[1], index[pair]))
            queue.append((pair, -index[pair]))
    while queue:
        apply(*queue.popleft())

char_count = defaultdict(int)


items = list(index.items())

log(items)

for pair, occurrences in items:
    char_count[pair[0]] += occurrences
    char_count[pair[1]] += occurrences

edges = (template[0], template[-1])

for char in char_count.keys():
    char_count[char] = char_count[char] // 2    # we counted every char twice
    if char in edges:                           # except for the two edge characters
        char_count[char] += 1

log(char_count)

min_char = min(char_count.keys(), key=lambda c: char_count[c])
max_char = max(char_count.keys(), key=lambda c: char_count[c])
min_count = char_count[min_char]
max_count = char_count[max_char]

print(f'After {steps} steps:')
print(f'Most frequent character is {min_char}: {min_count} occurrences')
print(f'Least frequent character is {max_char}: {max_count} occurrences')
print(f'Diff: {max_count - min_count}')

