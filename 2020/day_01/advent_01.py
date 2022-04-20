#!/usr/bin/python3

import sys
from functools import reduce

lines = sys.stdin.readlines()
values = sorted([int(line[:-1]) for line in lines])
values_set = set(values)

idx = {v: i for i, v in enumerate(values)}
i = 0
for v in range(0, 2020):
    i = idx.get(v, i)
    idx[v] = i

printed = set()

def answer(*values):
    tupl = tuple(sorted(values))
    if tupl not in printed:
        printed.add(tupl)
        expr = ' * '.join([str(v) for v in tupl])
        print(f'{expr} = {reduce(lambda acc, v: acc * v, tupl)}')

for first in values:
    # part 1
    second = 2020 - first
    if second > 0 and second in values_set:
        answer(first, second)
    # part 2
    for i in range(idx[2020 - first] - 1, -1, -1):
        second = values[i]
        third = 2020 - first - second
        if third > 0 and third in values_set:
            answer(first, second, third)

