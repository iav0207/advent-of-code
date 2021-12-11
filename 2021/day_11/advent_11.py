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
    step = 0
    flashes_100 = 0

    while len(flashed) < n**2: # until they all flash
        print_f(step)
        flashed.clear()

        make_step()

        step += 1
        flashes_100 += len(flashed) if step <= 100 else 0

    print_f(step)
    print(f'Total flashes after step 100: {flashes_100}')
    print(f'First sync at step {step}')


def make_step():
    foreach(lambda i, j, _: enqueue(i, j))
    while queue:
        process(dequeue())
    foreach(lambda i, j, it: reset(i, j) if it > 9 else None)


def enqueue(i, j):
    queue.append((i, j))


def dequeue():
    return queue.popleft()


def process(i, j):
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


def print_f(step):
    if not debug:
        return
    print(f'\nAfter step {step}:')
    for i in range(len(f)):
        print(''.join([str(it) for it in f[i]]))



if __name__ == '__main__':
    main()

