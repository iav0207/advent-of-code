#!/usr/bin/python3

import sys
from functools import reduce

field = [line[:-1] for line in sys.stdin]
n, m = len(field), len(field[0])

slopes = [
    (1, 1),
    (1, 3),
    (1, 5),
    (1, 7),
    (2, 1),
]
trees_count = [0 for _ in slopes]

for si, (di, dj) in enumerate(slopes):
    for i in range(0, n, di):
        j = (dj*i//di) % m
        trees_count[si] += 1 if field[i][j] == '#' else 0

for slope, count in zip(slopes, trees_count):
    print(f'Slope {slope}: {count}')

print(f'Answer: {reduce(lambda acc, it: acc * it, trees_count)}')

