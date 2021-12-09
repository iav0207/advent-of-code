#!/usr/bin/python3

import sys
from collections import defaultdict

cost_increment = int(sys.argv[1] or 0)

input = sys.stdin.read()
pos = sorted([int(it) for it in input.split(',')])
idx = defaultdict(int)
for p in pos:
    idx[p] += 1

min_cost = 1e16
lo, hi = 0, len(pos)
while lo <= hi:
    mid = lo + ((hi - lo) // 2)
    dest = pos[mid]
    cost = 0
    for p, amount in idx.items():
        length = abs(p - dest)
        each_cost = int(length * (2 + cost_increment * (length - 1)) / 2)
        group_cost = amount * each_cost
        cost += group_cost
    if cost < min_cost
    min_cost = cost

if cost > min_cost:
    print(f'Destination {dest}\nCost {min_cost}')
    exit()

