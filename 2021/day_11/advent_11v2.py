#!/usr/bin/python3

import sys
from collections import deque

field = [[int(c) for c in line] for line in sys.stdin.read().splitlines()]
n, m = len(field), len(field[0])
step, flashes = 0, 0
done = [False, False]

def adj(i, j):
    for ii in range(max(0, i-1), min(n, i+2)):
        for jj in range(max(0, j-1), min(m, j+2)):
            if ii != i or jj != j:
                yield ii, jj

while True:
    step += 1
    todo = deque()
    flashed = set()

    for i in range(n):
        for j in range(m):
            field[i][j] += 1
            if field[i][j] > 9:
                todo.append((i, j))
    while todo:
        flash = todo.popleft()
        if flash in flashed:
            continue
        flashed.add(flash)
        for i, j in adj(*flash):
            field[i][j] += 1
            if field[i][j] > 9:
                todo.append((i, j))

    for i, j in flashed:
        field[i][j] = 0
    flashes += len(flashed)

    if not done[0] and step == 100:
        print(f"Part 1: {flashes}")
        done[0] = True
    if not done[1] and len(flashed) == n*m:
        print(f"Part 2: {step}")
        done[1] = True
    if all(done):
        break

