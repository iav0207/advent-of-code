#!/usr/bin/python3

import sys
from functools import reduce

lines = sys.stdin.readlines()
values = sorted([int(line[:-1]) for line in lines])
values_set = set(values)

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
    for second in values:
        if second > 2020 - first:
            break
        third = 2020 - first - second
        if third > 0 and third in values_set:
            answer(first, second, third)

