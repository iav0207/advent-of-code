#!/usr/bin/python3

import sys


lines = [line.replace('\n', '') for line in sys.stdin.readlines()]

algorithm = lines[0]

debug = False
steps = 50
extra_cells_per_step = 4
margin = steps * extra_cells_per_step
x_margin = '.' * margin

image = [x_margin + line + x_margin for line in lines[2:]]

empty_row = '.' * len(image[0])
y_margin = [empty_row] * margin

image = y_margin + image + y_margin

n, m = len(image), len(image[0])


def square_at(img, row, col, default):
    for i in range(row - 1, row + 2):
        for j in range(col - 1, col + 2):
            yield img[i][j] if (0 <= i < n and 0 <= j < m) else default


def int_at(img, row, col, default):  # reads the binary encoding of the square
    return int(''.join(['1' if c == '#' else '0' for c in square_at(img, row, col, default)]), 2)


def produce_new_from(old, default='.'):
    new = []
    for i in range(n):
        new.append([])
        for j in range(m):
            new[i].append(algorithm[int_at(old, i, j, default)])
    return [''.join(row) for row in new]


for step in range(steps):
    border = '.' if step % 2 == 0 else '#'
    out = produce_new_from(image, border)
    image = out
    if debug:
        print('\n'.join(out))
    if step == 1 or step == steps - 1:
        lit = sum(sum(1 if c == "#" else 0 for c in row) for row in out)
        print(f'{lit} pixels are lit after step {step + 1}.')
