#!/usr/bin/python3

import sys
from itertools import product, repeat
from multiprocessing import Pool

import numpy as np

lines = [line.replace('\n', '').split(' ') for line in sys.stdin.readlines() if line]
instructions = [line[0] for line in lines]
cubes = [line[1].split(',') for line in lines]


# take all distinct values, put in a list. for x, y, z
# build an index of lengths x[i+1] - x[i] -> xi
# build a 3d matrix, turn on and off at (xi, yi, zi)

uniqx = set()
uniqy = set()
uniqz = set()

for i, cube in enumerate(cubes):
    x = tuple(int(c) for c in cube[0].split('=')[1].split('..'))
    y = tuple(int(c) for c in cube[1].split('=')[1].split('..'))
    z = tuple(int(c) for c in cube[2].split('=')[1].split('..'))
    uniqx.add(int(x[0]))
    # uniqx.add(int(x[0]) - 1)
    # uniqx.add(int(x[0]) + 1)
    uniqx.add(int(x[1]))
    # uniqx.add(int(x[1]) - 1)
    uniqx.add(int(x[1]) + 1)
    uniqy.add(int(y[0]))
    # uniqy.add(int(y[0]) - 1)
    # uniqy.add(int(y[0]) + 1)
    uniqy.add(int(y[1]))
    # uniqy.add(int(y[1]) - 1)
    uniqy.add(int(y[1]) + 1)
    uniqz.add(int(z[0]))
    # uniqz.add(int(z[0]) - 1)
    # uniqz.add(int(z[0]) + 1)
    uniqz.add(int(z[1]))
    # uniqz.add(int(z[1]) - 1)
    uniqz.add(int(z[1]) + 1)

X, Y, Z = 'x', 'y', 'z'
idx = np.full((3, max(len(uniqx), len(uniqy), len(uniqz))), 0, dtype=int)

def diff_index(crds, dimension):
    arr = sorted(crds)
    for i, val in enumerate(arr):
        idx[dimension][i] = arr[i + 1] - val if i + 2 < len(arr) else 1

diff_index(uniqx, 0)
diff_index(uniqy, 1)
diff_index(uniqz, 2)

# idx = {
#     X: {i: val for i, val in enumerate(sorted(uniqx))},
#     Y: {i: val for i, val in enumerate(sorted(uniqy))},
#     Z: {i: val for i, val in enumerate(sorted(uniqz))},
# }
rev_idx = {
    X: {val: i for i, val in enumerate(sorted(uniqx))},
    Y: {val: i for i, val in enumerate(sorted(uniqy))},
    Z: {val: i for i, val in enumerate(sorted(uniqz))},
}

dim = len(rev_idx[X]), len(rev_idx[Y]), len(rev_idx[Z])
lit = np.full(dim, False, dtype=bool)  # i, j, k says if cube i:i+1,j:j+1,k:k+1 is lit
vol = np.full(dim, 0, dtype=int)


# def print_lit():
#     for i, j, k in product(range(dim[0]), range(dim[1]), range(dim[2])):
#         if lit[i, j, k]:
#             print(','.join([str(idx[X][i]), str(idx[Y][j]), str(idx[Z][k])]))


part1 = True

for instruction, cube in zip(instructions, cubes):
    xi = tuple(rev_idx[X][int(coord)] for coord in cube[0].split('=')[1].split('..'))
    yi = tuple(rev_idx[Y][int(coord)] for coord in cube[1].split('=')[1].split('..'))
    zi = tuple(rev_idx[Z][int(coord)] for coord in cube[2].split('=')[1].split('..'))
    # if part1 and any(abs(it) > 50 for it in [ (*xi, *yi, *zi)]):
    #     continue
    lit[min(xi):max(xi) + 1, min(yi):max(yi) + 1, min(zi):max(zi) + 1] = instruction == 'on'
    # print(f'{instruction} {cube}')
    # print_lit()


vol = 0

print(idx)

def solve(local_lit, local_idx, start_i):
    todo = local_lit.size
    done = 0
    print(f'NEW WORKER: shape {local_lit.shape} start i = {start_i:>5}. To do: {local_lit.size:>10}')
    vol = 0
    range3 = product(*tuple(range(it) for it in local_lit.shape))
    for i, j, k in range3:
        # if j == 0 and k == 0:
        #     print(f'i = {i}')
        #     print(f'i + start_i = {i + start_i}')
        # print(f'(i,j,k)={(i, j, k)}')
        done += 1
        if start_i == 0 and done % 1_000_000 == 0:
            print(f'Done {done*100/todo:2.2}%')
        dx = local_idx[0, start_i + i] # if start_i + i + 2 < lit.shape[0] else 1
        dy = local_idx[1, j] #  if j + 2 < local_lit.shape[1] else 1
        dz = local_idx[2, k] #  if k + 2 < local_lit.shape[2] else 1
        this = local_lit[i, j, k]
        if this:
            # print(f'lit')
            vol += dx * dy * dz
            continue
    return vol

if __name__ == '__main__':
    pool_size = int(sys.argv[1] if len(sys.argv) > 1 else 1)
    print(f'Amount of work: {lit.size} 3d elements.')
    if pool_size > 1:
        print(f'Sharing the job: spawning {pool_size} workers')
    with Pool(pool_size) as p:
        litsplit = np.array_split(lit, pool_size)
        start_is = [0]*pool_size
        for i in range(1, pool_size):
            start_is[i] = start_is[i-1] + litsplit[i-1].shape[0]
        vv = p.starmap(solve, zip(litsplit, repeat(idx), start_is))
    print(f'{sum(vv)} cubes are lit.')

# volume = for i, j, k sum of

# lit = np.ndarray((101, 101, 101), dtype=bool)
# for instruction, cube in zip(instructions, cubes):
#     x = tuple(int(coord)+50 for coord in cube[0].split('=')[1].split('..'))
#     y = tuple(int(coord)+50 for coord in cube[1].split('=')[1].split('..'))
#     z = tuple(int(coord)+50 for coord in cube[2].split('=')[1].split('..'))
#     if all(abs(it) <= 101 for it in chain(x, y, z)):
#         lit[min(x):max(x) + 1, min(y):max(y) + 1, min(z):max(z) + 1] = instruction == 'on'
#
#
# print(f'Part 1: {lit.sum()} cubes are lit.')

