#!/usr/bin/python3

import sys
import numpy as np
from itertools import product

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))

jets = sys.stdin.read()[:-1]
j = 0

# Each rock appears so that its left edge is two units away from the left wall
# and its bottom edge is three units above the highest rock in the room
# (or the floor, if there isn't one).
shapes = [
    frozenset((x, 4) for x in range(2, 6)),
    frozenset({(x, 5) for x in range(2, 5)}.union({(3, 4), (3, 6)})),
    frozenset({(x, 4) for x in range(2, 5)}.union({(4, 5), (4, 6)})),
    frozenset((2, y) for y in range(4, 8)),
    frozenset((x, y) for x, y in product(range(2, 4), range(4, 6))),
]
shid = 0  # shape id

chamber_x = range(7)

rocks = set()
top = 0  # the height of the structure


def shift(shape, shift):
    dx, dy = shift
    return {(x + dx, y + dy) for x, y in shape}


move_left = lambda s: shift(s, (-1, 0))
move_right = lambda s: shift(s, (1, 0))
move_down = lambda s: shift(s, (0, -1))

within_chamber_x = lambda shape: all(x in chamber_x for x, _ in shape)
hits_the_floor = lambda shape: any(y == 0 for _, y in shape)
hits_a_rock = lambda shape: any(point in rocks for point in shape)


def generate_shape():
    global shid
    shape = shapes[shid]
    shape = shift(shape, (0, top))
    shid = (shid + 1) % len(shapes)
    return shape


def generate_jet():
    global j
    jet = jets[j]
    j = (j + 1) % len(jets)
    return jet


def launch_a_shape():
    global top
    shape = generate_shape()
    # debug(f'spawned {shape}')
    while True:
        jet = generate_jet()
        candidate = move_left(shape) if jet == '<' else move_right(shape)
        if within_chamber_x(candidate) and not hits_the_floor(candidate) and not hits_a_rock(candidate):
            # debug(f'Push {jet}')
            shape = candidate
        candidate = move_down(shape)
        if hits_the_floor(candidate) or hits_a_rock(candidate):
            break
        # debug('Moving down')
        shape = candidate
    top = max(top, max(y for _, y in shape))
    for point in shape:
        rocks.add(point)


for _ in range(2022):
    launch_a_shape()

print(f'Part 1: the tower is {top} units tall')


rocks.clear()
top = 0
tops = [0]
total_steps = 1000000000000
j = 0
shid = 0

# The idea is that once we hit a state we've been in already
# the simulation will run identically, so we can fast-forward the cycles.
# We consider the state to be the same if:
# - we're about to launch the same shape
# - the jet "id" is the same
# - and the rocks at the top look similarly (the profile is the same as we've already seen)
# So we maintain a lookup table of the states:

memory = dict()  # (profile, shid, j) to (step, top)
capture_state = lambda: (capture_profile(), shid, j)


def capture_profile():
    """Takes the picture of the rocks profile at the top"""
    xs = set()  # covered `x` coordinates
    profile = set()
    ymin, ymax = top, top
    yrange = range(ymax, -1, -1)
    for y in yrange:
        for x in chamber_x:
            if (x, y) in rocks:
                profile.add((x, y))
                xs.add(x)
                ymin = min(ymin, y)
        if len(xs) == len(chamber_x):  # exit criterion: the profile covers all `x` coordinates
            break
    return frozenset((x, y - ymin) for x, y in profile)  # normalizing `y` coordinates to zero-based


def draw_profile(profile):
    ymax, ymin = max(y for _, y in profile), min(y for _, y in profile)
    for y in range(ymax, ymin - 1, -1):
        debug('|' + ''.join(['#' if (x, y) in profile else ' ' for x in chamber_x]) + '|')


def calculate_final_top_using(older_twin_state):
    older_twin_step, older_twin_top = older_twin_state
    print(f'Bingo at step {step} top {top}, older: {older_twin_step}, {older_twin_top}')
    cycle_height = top - older_twin_top
    steps_left = total_steps - step
    cycle_steps = step - older_twin_step
    cycles_left = int(steps_left / cycle_steps)
    debug(f'cycles_left = {cycles_left}, cycle_steps = {cycle_steps}, cycle_height = {cycle_height}')
    debug(f'total height added from full cycles: {cycle_height * cycles_left}')
    extra_steps_to_go = steps_left % cycle_steps
    extra_height = tops[older_twin_step + extra_steps_to_go] - tops[older_twin_step]
    debug(f'extra steps to go: {extra_steps_to_go}, adding height {extra_height}')
    return top + cycle_height * cycles_left + extra_height


for step in range(total_steps):
    state = capture_state()
    if state in memory:
        draw_profile(state[0])
        debug('\nShape:\n')
        draw_profile(shapes[shid])
        top = calculate_final_top_using(memory[state])
        break
    else:
        memory[state] = step, top

    launch_a_shape()
    tops.append(top)
    debug(f'{step:>4}. {shid} {j:<4}')


print(f'Part 2: the tower is {top} units tall')

