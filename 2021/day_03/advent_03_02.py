#!/usr/bin/python3

import sys

lines = sorted(sys.stdin.readlines())


def process_column(c, from_incl, to_excl, prefer):
    if to_excl - from_incl < 2:
        return from_incl, to_excl
    ones_from_idx = next((i for i in range(from_incl, to_excl) if lines[i][c] == '1'), -1)
    zeros, ones = ones_from_idx - from_incl, to_excl - ones_from_idx
    next_range = (from_incl, ones_from_idx) if (prefer == 1) ^ (ones >= zeros) else (ones_from_idx, to_excl)
    return process_column(c+1, next_range[0], next_range[1], prefer)


oxy_idx = process_column(0, 0, len(lines), prefer=1)[0]
co2_idx = process_column(0, 0, len(lines), prefer=0)[0]
oxy = int(lines[oxy_idx], 2)
co2 = int(lines[co2_idx], 2)
print(oxy * co2)
