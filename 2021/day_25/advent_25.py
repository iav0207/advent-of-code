#!/usr/bin/python3

import sys
from itertools import *
import numpy as np

debug = '-d' in sys.argv


def log(o):
    if debug:
        print(o)


EMPTY, SOUTH, EAST = '.v>'

lines = [line.replace('\n', '') for line in sys.stdin.readlines() if line]
n, m = len(lines), len(lines[0])

log(lines)

canmovesouth = set()    # loc
canmoveeast = set()     # loc
field = dict()          # loc -> symbol

def at(loc):
    return field.get(loc, EMPTY)

def west_from(loc):
    return loc[0], ((loc[1] - 1) + m) % m

def north_from(loc):
    return ((loc[0] - 1) + n) % n, loc[1]

def south_from(loc):
    return (loc[0] + 1) % n, loc[1]

def east_from(loc):
    return loc[0], (loc[1] + 1) % m

def log_field():
    for i in range(n):
        log(''.join([at((i, j)) for j in range(m)]))


for i, j in product(range(n), range(m)):
    this = lines[i][j]
    inorth, jnorth = north_from((i, j))
    iwest, jwest = west_from((i, j))
    if this == EMPTY:
        if lines[inorth][jnorth] == SOUTH:
            canmovesouth.add((inorth, jnorth))
        if lines[iwest][jwest] == EAST:
            canmoveeast.add((iwest, jwest))
    else:
        field[(i, j)] = this


def move(old_loc, herd):
    new_loc = east_from(old_loc) if herd == EAST else south_from(old_loc)
    cucu = field.pop(old_loc)
    assert cucu == herd
    assert at(new_loc) == EMPTY
    field[new_loc] = cucu

    if at(north_from(old_loc)) == SOUTH:
        canmovesouth.add(north_from(old_loc))
    if at(west_from(old_loc)) == EAST:
        canmoveeast.add(west_from(old_loc))

    if herd == EAST:
        canmoveeast.remove(old_loc)
        canmovesouth.discard(north_from(new_loc))
        if at(east_from(new_loc)) == EMPTY:
            canmoveeast.add(new_loc)
    else:
        canmovesouth.remove(old_loc)
        canmoveeast.discard(west_from(new_loc))
        if at(south_from(new_loc)) == EMPTY:
            canmovesouth.add(new_loc)
    

log(f'Initial state')
log_field()
step = 0
while canmoveeast or canmovesouth:
    step += 1
    for cucu_loc in list(canmoveeast):
        move(cucu_loc, EAST)
    for cucu_loc in list(canmovesouth):
        move(cucu_loc, SOUTH)
    log(f'After step {step}')
    log_field()
    log(f'Can move East: {canmoveeast}')
    log(f'Can move South: {canmovesouth}')

step += 1
print(f'Sea cucumbers stop moving at step {step}')

