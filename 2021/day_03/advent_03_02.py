#!/usr/bin/python3

import sys

lines = sorted(sys.stdin.readlines())


def process_column(c, from_incl, to_excl, prefer):
    if to_excl - from_incl < 2:
        return from_incl, to_excl
    ones_from_idx = next((i for i in range(from_incl, to_excl) if lines[i][c] == '1'), -1)
    zeros, ones = ones_from_idx - from_incl, to_excl - ones_from_idx
    if (prefer == 1) ^ (ones >= zeros):
        return process_column(c+1, from_incl, ones_from_idx, prefer) # considering zeros at this column
    else:
        return process_column(c+1, ones_from_idx, to_excl, prefer) # considering ones at this column


oxy_range = process_column(0, 0, len(lines), prefer=1)
co2_range = process_column(0, 0, len(lines), prefer=0)
oxy = int(lines[oxy_range[0]], 2)
co2 = int(lines[co2_range[0]], 2)
print(oxy * co2)

