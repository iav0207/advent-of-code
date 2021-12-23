#!/usr/bin/python3

import re
import sys
from itertools import product, repeat
from multiprocessing import Pool

import numpy as np

X, Y, Z = 0, 1, 2


# take all distinct values, put in a list. for x, y, z
# build an index of lengths x[i+1] - x[i] -> xi
# build a 3d array, turn on and off at (xi, yi, zi) as instructed by the input
# every lit region adds to the volume its dx*dy*dz looked up from the index

def main():
    instructions, cuboids, parallelism = read_inputs()

    intervals, coord_lookup, space_shape = compress_coordinates(cuboids)
    lit = process_instructions(instructions, cuboids, coord_lookup, space_shape)

    volume = calc_volume(lit, intervals, parallelism)

    print(f'{volume} cubes are lit.')


def read_inputs():
    parallelism = int(sys.argv[1] if len(sys.argv) > 1 else 1)
    lines = [line.replace('\n', '').split(' ') for line in sys.stdin.readlines() if line]
    instructions = [line[0] for line in lines]
    cuboids = []
    for line in lines:
        numbers = [int(it) for it in re.findall(r'[-\d]+', line[1])]
        cuboids.append(list(zip(numbers[0::2], numbers[1::2])))
    return instructions, cuboids, parallelism


def compress_coordinates(cuboids):
    uniq = set(), set(), set()

    for cuboid in cuboids:
        for dimension, (start, end) in enumerate(cuboid):
            uniq[dimension].add(start)
            uniq[dimension].add(end)
            uniq[dimension].add(end + 1)  # this is important, not 100% sure why, or how to avoid it:
                                          # it significantly adds to overall runtime

    space_shape = tuple(len(it) for it in uniq)

    intervals = np.full((3, max(space_shape)), 0, dtype=int)  # element [X, i] holds the length x[i+1] - x[i]
    coord_lookup = [dict(), dict(), dict()]  # coordinate lookup index: tells i by x[i]

    for dimension, coords in enumerate(uniq):
        sorted_coords = sorted(coords)
        for i, val in enumerate(sorted_coords):
            intervals[dimension, i] = sorted_coords[i + 1] - val if i + 1 < len(sorted_coords) else 1
            coord_lookup[dimension][val] = i

    print(intervals)
    return intervals, coord_lookup, space_shape


def process_instructions(instructions, cuboids, coord_lookup, shape):
    lit = np.full(shape, False, dtype=bool)  # i, j, k says if cuboid i, j, k is lit
    for instruction, (x, y, z) in zip(instructions, cuboids):
        xi = tuple(coord_lookup[X][coord] for coord in x)
        yi = tuple(coord_lookup[Y][coord] for coord in y)
        zi = tuple(coord_lookup[Z][coord] for coord in z)
        lit[min(xi):max(xi) + 1, min(yi):max(yi) + 1, min(zi):max(zi) + 1] = instruction == 'on'
    return lit


def calc_volume(lit, intervals, pool_size):
    print(f'Amount of work: {lit.size} 3d elements.')
    if pool_size < 2:
        return calc_partial_volume(lit, intervals)

    print(f'Sharing the job: spawning {pool_size} workers')
    chunks = np.array_split(lit, pool_size)
    start_is = [0] * pool_size
    for i in range(1, pool_size):
        start_is[i] = start_is[i-1] + chunks[i-1].shape[0]
    with Pool(pool_size) as pool:
        vv = pool.starmap(calc_partial_volume, zip(chunks, repeat(intervals), start_is))
    return sum(vv)


def calc_partial_volume(lit_chunk, intervals, start_i=0):
    todo = lit_chunk.size
    done = 0
    print(f'NEW WORKER: shape {lit_chunk.shape} start i = {start_i:>5}. To do: {todo:>10}')
    vol = 0
    for i, j, k in product(*tuple(range(it) for it in lit_chunk.shape)):
        done += 1
        if start_i == 0 and done % 10_000_000 == 0:
            print(f'Done {done*100/todo:2.2f}%')
        if not lit_chunk[i, j, k]:
            continue
        dx = intervals[X, start_i + i]
        dy = intervals[Y, j]
        dz = intervals[Z, k]
        vol += dx * dy * dz
    return vol


if __name__ == '__main__':
    main()

