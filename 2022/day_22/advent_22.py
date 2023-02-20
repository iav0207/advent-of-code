#!/usr/bin/env python3

import sys
import re
import math
from itertools import product

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()

field = lines[:-2]
instructions = lines[-1]

nrows = len(field)
ncols = max([len(line) for line in field])

facing = 'U'
pos = 0, field[0].index('.')

state = facing, pos

debug(pos)

instructions = re.findall(r'[LR]|\d*', instructions)
assert instructions[-1] == ''
assert 'R' not in instructions[0]
assert 'L' not in instructions[0]
instructions = ['R'] + instructions[:-1]

debug(instructions)

direc = 'RDLU'
next_inc = (
    (0, 1),     # R
    (1, 0),     # D
    (0, -1),    # L
    (-1, 0),    # U
)


def at(row, col):
    if row < 0 or col < 0:
        return ' '
    try:
        return field[row][col]
    except IndexError:
        return ' '


def rotate(facing, turn_r_or_l):
    facing_idx = direc.index(facing)
    facing_idx += 1 if  turn_r_or_l == 'R' else -1
    while facing_idx < 0:
        facing_idx += len(direc)
    facing_idx %= len(direc)
    return direc[facing_idx]


coord_increment = lambda facing: next_inc[direc.index(facing)]
add_vec = lambda a, b: tuple([ai + bi for ai, bi in zip(a, b)])
"""Element-wise product of vectors. Not a vector product."""
times_vec = lambda a, b: tuple([ai * bi for ai, bi in zip(a, b)])


def wrap(coord, facing):
    debug('flat wrap!')
    inc = coord_increment(facing)
    dec = times_vec((-1, -1), inc)
    cursor = coord
    while at(*cursor) != ' ':  # crawling back until we hit the void
        cursor = add_vec(cursor, dec)
    cursor = add_vec(cursor, inc)  # take one step forward into the field
    return cursor


def move(instruction):
    facing, (row, col) = state
    turn, moves = instruction
    facing = rotate(facing, turn)
    for i in range(int(moves)):
        inc = coord_increment(facing)
        nxt = add_vec((row, col), inc)
        if at(*nxt) == ' ':
            nxt = wrap((row, col), facing)
        if at(*nxt) == '#':
            break
        row, col = nxt
    return facing, (row, col)

for it in zip(instructions[0::2], instructions[1::2]):
    state = move(it)

facing, (row, col) = state
debug(f'row {row+1} col {col+1} fac {direc.index(facing)}')
result = 1000 * (row + 1) + 4 * (col + 1) + direc.index(facing)

print(f'Part 1: {result}')

# Part 2 plan: 1. find the face 2. apply coord transformations depending on the path to the face
# face rotations:
# starting face, always : (1, 0, 0)
rota = {
    'R': (0, 1, 0),
    'D': (0, 0, -1),
    'L': (0, -1, 0),
    'U': (0, 0, 1),
}

# edge length
total_surface = sum(1 if at(row, col) != ' ' else 0 for row, col in product(range(nrows), range(ncols)))
edge_length = int(math.sqrt(total_surface / 6))
debug(f'edge length is {edge_length}')

