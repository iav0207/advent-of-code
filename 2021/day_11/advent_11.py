#!/usr/bin/python3

import sys
from collections import deque

f = []

for line in sys.stdin.read().split('\n'):
    if line:
        f.append([int(c) for c in line])


n = len(f)
debug = False
queue = deque()
flashed = set()


def main():
    stepnum = 0
    first_sync = 0
    flashes_100 = 0
    print_f(stepnum)

    while not first_sync:
        flashed.clear()

        make_step()

        stepnum += 1
        print_f(stepnum)

        flashes_100 += len(flashed) if stepnum <= 100 else 0
        if len(flashed) == n**2:
            first_sync = stepnum

    print(f'Total flashes after step 100: {flashes_100}')
    print(f'First sync at step {first_sync}')


def make_step():
    foreach(lambda i, j, _: enqueue(i, j))
    while queue:
        dequeue()
    foreach(lambda i, j, it: reset(i, j) if it > 9 else None)


def enqueue(i, j):
    queue.append((i, j))


def dequeue():
    (i, j) = queue.popleft()
    if (i, j) in flashed:
        return
    f[i][j] += 1
    if f[i][j] == 10: # flash
        flashed.add((i, j))
        for x, y in [coord for coord in adjacent_to(i, j)]:
            enqueue(x, y)


def adjacent_to(i, j):
    for di, dj in square(range(-1, 2)):
        x, y = i + di, j + dj
        if 0 <= x < n and 0 <= y < n and (x, y) != (i, j):
            yield x, y


def reset(i, j):
    f[i][j] = 0


def foreach(do):
    for i, j in square():
        do(i, j, f[i][j])


def square(rng=range(n)):
    for i in rng:
        for j in rng:
            yield i, j


def print_f(stepnum):
    if not debug:
        return
    print(f'\nAfter step {stepnum}:')
    for i in range(len(f)):
        print(''.join([str(it) for it in f[i]]))



if __name__ == '__main__':
    main()

