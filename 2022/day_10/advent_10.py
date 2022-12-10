#!/usr/bin/python3

import sys

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()

cycles = [1]

for line in lines:
    if line == 'noop':
        cycles.append(cycles[-1])
    else:
        operand = int(line.split(' ')[1])
        cycles.extend([cycles[-1], cycles[-1] + operand])

interesting = [20, 60, 100, 140, 180, 220]

debug(cycles)

debug([cycles[i-1] for i in interesting])
strengths = [cycles[i-1] * i for i in interesting]

print(f'Part 1: {sum(strengths)}')

print('Part 2:')

lit = set()
for i, sprite_pos in enumerate(cycles):
    x, y = i % 40, int(i / 40)
    debug(f'x,y = {x}, {y}')
    if x in range(sprite_pos - 1, sprite_pos + 2):
        lit.add((x, y))

for y in range(6):
    print(''.join(['#' if (x,y) in lit else '.' for x in range(40)]))

