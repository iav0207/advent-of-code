#!/usr/bin/env python3

import sys
import functools

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


source = sys.stdin.read().replace('\n', '')

row_count, col_count = 6, 25
lay_length = row_count * col_count
lay_count = int(len(source) / lay_length)
row_length = int(lay_length / row_count)

debug(f'len={len(source)}, {lay_count} layers')

def count(series, digit):
    return sum(1 if it == digit else 0 for it in series)


layers = []
min_zeros, min_zeros_layer_num = lay_length + 1, 0
for li in range(lay_count):
    layer = source[li*lay_length:(li+1)*lay_length]
    layers.append(layer)
    zero_count = count(layer, '0')
    if zero_count < min_zeros:
        min_zeros, min_zeros_layer_num = zero_count, li

winner = layers[min_zeros_layer_num]
ones, twos = count(winner, '1'), count(winner, '2')

print(f'Part 1: {ones * twos}')
print('Part 2 follows:\n')

for row in range(row_count):
    img_row = '  '
    for col in range(col_count):
        ray = ''.join([layers[li][row*row_length + col] for li in range(lay_count)])
        pxl = functools.reduce(lambda a, b: b if a == '2' else a, ray, '2')
        img_row += 'X' if pxl == '1' else ' '
    print(img_row)

