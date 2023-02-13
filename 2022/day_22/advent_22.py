#!/usr/bin/env python3

import sys
import re

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


def move(instruction):
    facing, (row, col) = state
    turn, moves = instruction
    moves = int(moves)
    facing_idx = direc.index(facing)
    facing_idx += 1 if turn == 'R' else -1
    while facing_idx < 0:
        facing_idx += len(direc)
    facing_idx %= len(direc)
    facing = direc[facing_idx]
    for i in range(moves):
        inc = next_inc[facing_idx]
        nxt = row + inc[0], col + inc[1]
        if at(*nxt) == ' ':
            debug('wrap!')
            cursor = row, col
            while at(*cursor) != ' ':
                cursor = cursor[0] - inc[0], cursor[1] - inc[1]
            cursor = cursor[0] + inc[0], cursor[1] + inc[1]
            debug(cursor)
            nxt = cursor
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

