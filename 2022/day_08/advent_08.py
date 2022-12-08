#!/usr/bin/python3

import sys
from functools import reduce

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()
arr = [[int(c) for c in line] for line in lines]

debug(arr)

visible = set()

# axis == list of coords

def look_from(axis, increment):
    for c in axis:
        visible.add(c)
    maxv = [arr[c[0]][c[1]] for c in axis]
    while True:
        try:
            axis = [(c[0] + increment[0], c[1] + increment[1]) for c in axis]
            i = 0
            for c in axis:
                if arr[c[0]][c[1]] > maxv[i]:
                    visible.add(c)
                    maxv[i] = arr[c[0]][c[1]]
                i += 1
        except IndexError:
            break


look_from([(x, 0) for x in range(0, len(arr))], (0, 1))
look_from([(x, len(arr[0])-1) for x in range(0, len(arr))],(0, -1))
look_from([(0, y) for y in range(0, len(arr[0]))],(1, 0))
look_from([(len(arr)-1, y) for y in range(0, len(arr[0]))],(-1, 0))

print(f'Part 1: {len(visible)} trees are visible')


def all_coords():
    for x in range(0, len(arr)):
        for y in range(0, len(arr[0])):
            yield x, y


def scenic_score(view_distances):
    score = reduce(lambda acc, it: acc * it, view_distances)
    debug(f'{score} = {"*".join(str(it) for it in view_distances)}')
    return score


def find_view_distance(from_c, increment):
    c = from_c
    from_h = arr[from_c[0]][from_c[1]]
    dist = 0
    while True:
        try:
            c = (c[0] + increment[0], c[1] + increment[1])
            if c[0] < 0 or c[1] < 0:
                break
            h = arr[c[0]][c[1]]
            dist += 1
            if h >= from_h:
                return dist
        except IndexError:
            break
    return dist


directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    

def find_view_distances_from(c0):
    debug(f'fvdf {c0} h = {arr[c0[0]][c0[1]]}')
    return [find_view_distance(c0, inc) for inc in directions]


max_score = max(scenic_score(find_view_distances_from(c)) for c in all_coords())
print(f'Part 2: Max scenic score is {max_score}')

