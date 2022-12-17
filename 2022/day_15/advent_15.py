#!/usr/bin/python3

import sys
import re
from collections import deque

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


all_ints = [int(it) for it in re.findall(r'-?\d+', sys.stdin.read())]
debug(all_ints)

sensors = list(zip(all_ints[::4], all_ints[1::4]))
beacons = list(zip(all_ints[2::4], all_ints[3::4]))
sens = set(sensors)
beac = set(beacons)


def manhattan(p1, p2):
    return abs(p2[0] - p1[0]) + abs(p2[1] - p1[1])


radia = [manhattan(s, b) for s, b in zip(sensors, beacons)]


if False:  # runs slow for the real input. replaced with the implementation that follows
    debug('Evaluating coverage...')
    
    covered = set()
    def evaluate_coverage():
        for s, b in zip(sensors, beacons):
            max_dist = manhattan(s, b)
            debug(f'Max dist is {max_dist}')
            for x in range(s[0] - max_dist, s[0] + max_dist + 1):
                for y in range(s[1] - max_dist, s[1] + max_dist + 1):
                    if manhattan((x, y), s) <= max_dist:
                        yield x, y
    
                    
    covered = set(evaluate_coverage())
    
    debug(f'{len(covered)} points are covered by sensors')


def eval_covered_intervals_at_y(y):
    # each sensor 'projects' its coverage onto the line at y
    # a projection is encoded as (center, half-width)
    projections = list()
    # evaluating sensor coverage projections
    for i, s, b in zip(range(len(sensors)), sensors, beacons):
        max_dist = radia[i]
        projection = (s[0], max(0, max_dist - abs(s[1] - y)))
        debug(f'i={i}, s={s}, b={b}, max_dist={max_dist} proj={projection}')
        projections.append(projection)
    # intersecting the projections
    intervals = sorted([(pc - pd, pc + pd) for pc, pd in projections], key=lambda it: it[0])
    debug(f'intervals: {intervals}')
    union = [intervals[0]]
    coverage = intervals[0][1] - intervals[0][0]
    for interval in intervals[1:]:
        debug(f'union = {union}')
        debug(f'interval = {interval}')
        if interval[0] <= union[-1][1]:
            union[-1] = union[-1][0], max(union[-1][1], interval[1])
        else:
            union.append(interval)
    debug(f'union = {union}')
    return union


def count_covered_at_y(y):
    coverage = sum(it[1] - it[0] + 1 for it in eval_covered_intervals_at_y(y))
    debug(f'coverage = {coverage}')
    return coverage - len(set(it[0] for it in beacons if it[1] == y))


print(f'At y=10 {count_covered_at_y(10)} points are covered by sensors')
print(f'At y=2000000 {count_covered_at_y(2000000)} points are covered by sensors')
# 5832528

limits = range(4000001)
limits = range(20)


for y in range(2000000 - 10, 2000000 + 11):
    debug_mode = False
    cov = eval_covered_intervals_at_y(y)
    debug_mode = True
    debug(f'{y:>12}: {cov}')


y = 2000000
debug_mode = False
for dy in range(2000000):
    for yy in y - dy, y + dy:
        covered = eval_covered_intervals_at_y(yy)
        if any(it for it in covered if it[0] == it[1]):
            zero_width = next(it for it in covered if it[0] == it[1])
            print(f'zero-width interval at y = {yy}, x = {zero_width[0]}')
            exit(0)


def tuning_frequency(x, y):
    return 4000000 * x + y

