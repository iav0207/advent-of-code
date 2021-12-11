#!/usr/bin/python3

import sys
from collections import defaultdict

cost_increment = int(sys.argv[1] or 0)

input = sys.stdin.read()
pos = sorted([int(it) for it in input.split(',')])
idx = defaultdict(int)
for p in pos:
    idx[p] += 1


def cost_to(dest):
    cost = 0
    for p, amount in idx.items():
        length = abs(p - dest)
        each_cost = int(length * (2 + cost_increment * (length - 1)) / 2)
        group_cost = amount * each_cost
        cost += group_cost
    return cost


min_cost = 1e16
lo, hi = 0, len(pos)
delta = None

while lo < hi:
    mid = lo + ((hi - lo) // 2)
    mid_left = mid  # looking for first distinct dest to the left
    while pos[mid_left] == pos[mid]:
        mid_left -= 1

    mid_cost = cost_to(pos[mid])
    if min_cost > mid_cost:
        destination = pos[mid]
        min_cost = mid_cost

    delta = mid_cost - cost_to(pos[mid_left])
    hi = mid if delta >= 0 else hi
    lo = mid + 1 if delta < 0 else lo

print(f'Destination {destination}\nCost {min_cost}')

