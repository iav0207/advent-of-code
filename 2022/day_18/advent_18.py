#!/usr/bin/env python3

import sys
import numpy as np
from itertools import product
from collections import deque

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()
cubes = [tuple(int(num) for num in line.split(',')) for line in lines]

xmin, xmax, ymin, ymax, zmin, zmax = [None] * 6

for cube in cubes:
    if xmin is None:
        xmin, ymin, zmin = cube
        xmax, ymax, zmax = cube
        continue
    x, y, z = cube
    xmin = min(xmin, x)
    ymin = min(ymin, y)
    zmin = min(zmin, z)
    xmax = max(xmax, x)
    ymax = max(ymax, y)
    zmax = max(zmax, z)

debug((xmin, xmax, ymin, ymax, zmin, zmax))
xrange = range(xmin, xmax + 1)
yrange = range(ymin, ymax + 1)
zrange = range(zmin, zmax + 1)
ranges = xrange, yrange, zrange

grid = np.zeros((xmax + 3, ymax + 3, zmax + 3), dtype=bool)
for cube in cubes:
    grid[cube] = True

is_within_bounds = lambda point: all(coord in rng for coord, rng in zip(point, ranges))

def neighbors(cube):
    for i in range(6):
        direction = 1 if i % 2 == 0 else -1
        axis = int(i / 2)
        neighbor = list(cube)
        neighbor[axis] += direction
        if is_within_bounds(neighbor):
            yield tuple(neighbor)


area = 0
for point in product(*ranges):
    if not grid[point]:
        continue
    connected_count = sum(1 if grid[n] else 0 for n in neighbors(point))
    open_count = 6 - connected_count
    area += open_count

print(f'Part 1: {area}')

area = 0
seen = set()
queue = deque()


def enqueue(p):
    if p in seen:
        return
    seen.add(p)
    queue.append(p)


def process(*point):
    global area
    if grid[point]:
        area += 1
    else:
        enqueue(point)


for x, y in product(xrange, yrange):
    process(x, y, 0)
    process(x, y, zmax + 1)
for y, z in product(yrange, zrange):
    process(0, y, z)
    process(xmax + 1, y, z)
for z, x in product(zrange, xrange):
    process(x, 0, z)
    process(x, ymax + 1, z)

while queue:
    for n in neighbors(queue.popleft()):
        process(*n)

print(f'Part 2: {area}')

