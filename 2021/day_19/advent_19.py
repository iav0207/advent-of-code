#!/usr/bin/python3

from itertools import *
import sys
import json
from collections import defaultdict

lines = [line.replace('\n', '') for line in sys.stdin.readlines()]
dimensions = len(lines[1].split(','))


debug = '-d' in sys.argv


def log(something):
    if not debug:
        return
    if isinstance(something, str) or isinstance(something, int):
        print(something)
    elif isinstance(something, list):
        for it in something:
            print(it)
    else:
        print(json.dumps(something, indent=2))


def log_points(ps):
    if not debug:
        return
    for p in ps:
        print(''.join([str(coord).rjust(6, ' ') for coord in p]))


def add(vec1, vec2):
    return tuple(a + b for a, b in zip(vec1, vec2))


def diff(vec1, vec2):
    return tuple(a - b for a, b in zip(vec1, vec2))



class Scanner:
    def __init__(self, num, readings=None, transform=0):
        self.num = num
        self.readings = readings or []
        self.transform = transform

    def print_out(self, adjustment=None):
        log(f'Scanner {self.num}')
        readings = [] if adjustment else self.readings
        if adjustment:
            for r in self.readings:
                readings.append(add(adjustment, r))
        log_points(readings)
        yield from readings

    def __repr__(self):
        return f'Scanner(id={self.num},tr={self.transform})'


class Transform:
    @staticmethod
    def scanner(s: Scanner):
        p_generators = [Transform.point(p) for p in s.readings]
        for t in range(24):
            yield Scanner(s.num, [next(g) for g in p_generators], transform=t)

    # In total, each scanner could be in any of 24 different orientations:
    # facing positive or negative x, y, or z,
    # and considering any of four directions "up" from that facing.
    @staticmethod
    def point(p):
        x, y, z = p
        # The rotations took me like 5 attempts to write properly
        yield  x,  y,  z
        yield  x, -z,  y
        yield  x, -y, -z
        yield  x,  z, -y
        yield -x,  y, -z
        yield -x,  z,  y
        yield -x, -y,  z
        yield -x, -z, -y

        yield  y,  z,  x
        yield  y, -x,  z
        yield  y, -z, -x
        yield  y,  x, -z
        yield -y,  z, -x
        yield -y,  x,  z
        yield -y, -z,  x
        yield -y, -x, -z

        yield  z,  x,  y
        yield  z, -y,  x
        yield  z, -x, -y
        yield  z,  y, -x
        yield -z,  x, -y
        yield -z,  y,  x
        yield -z, -x,  y
        yield -z, -y, -x


scanners = []

for line in lines:
    if 'scanner' in line:
        snum = int(line.split(' ')[2])
        readings = []
    elif not line:
        scanners.append(Scanner(snum, readings))
    else:
        readings.append(tuple(int(c) for c in line.split(',')))
scanners.append(Scanner(snum, readings))


right = [0]  # the ones that we count as oriented right, in the sense of 'correctly'
left = list(range(1, len(scanners)))  # not yet oriented. we're going to align them all with the first scanner
intersection_thresh = min(12, len(scanners[0].readings))

beacons_from_0 = set(scanners[0].readings)
scanners_from_0 = [(0, 0, 0) if i == 0 else None for i in range(len(scanners))]

def find(generator):
    return next((it for it in generator if it is not None), None)


def find_left_scanner_position(left_scanner: Scanner, right_scanner: Scanner) -> tuple:
    match = defaultdict(int)  # dict: (diff) -> (count)
    for left_reading, right_reading in product(left_scanner.readings, right_scanner.readings):
        match[diff(right_reading, left_reading)] += 1
    position, match_count = max(match.items(), key=lambda it: it[1])
    if match_count >= intersection_thresh:
        log(f'Max match: {match_count}')
        return position


def find_adjustment(left_scanner: Scanner) -> [Scanner, tuple]:
    right_scanners = [scanners[r] for r in right]
    configurations = product(Transform.scanner(left_scanner), right_scanners)
    for left_scanner_rotated, right_scanner in configurations:
        left_scanner_position = find_left_scanner_position(left_scanner_rotated, right_scanner)
        if left_scanner_position:
            left_scanner_position = add(scanners_from_0[right_scanner.num], left_scanner_position)
            return left_scanner_rotated, left_scanner_position


while left:
    found = find(find_adjustment(scanners[le]) for le in left)

    log(f'\nright {right}\nleft {left}\nbeacons identified {len(beacons_from_0)}')
    assert found, 'Not found :('

    adjusted_scanner, its_position = found

    assert scanners_from_0[adjusted_scanner.num] is None
    assert adjusted_scanner
    assert its_position

    scanners_from_0[adjusted_scanner.num] = its_position
    left.remove(adjusted_scanner.num)
    right.append(adjusted_scanner.num)
    scanners[adjusted_scanner.num] = adjusted_scanner  # important: put adjusted readings of the normalized scanner
    new_beacons = [add(its_position, beacon) for beacon in adjusted_scanner.readings]
    beacons_from_0.update(new_beacons)

    log(f'Overlapping beacons, relative to scanner 0:')
    log_points(new_beacons)
    log(f'{len(left)} scanners are left to identify. {len(beacons_from_0)} beacons found so far.')


print(f'The scanners see {len(beacons_from_0)} beacons in total.')

max_manhattan = 0

for i, j in product(range(len(scanners)), range(len(scanners))):
    if i == j:
        continue
    xi, yi, zi = scanners_from_0[i]
    xj, yj, zj = scanners_from_0[j]
    max_manhattan = max(max_manhattan, abs(xi - xj) + abs(yi - yj) + abs(zi - zj))

print(f'Max Manhattan distance between two scanners is {max_manhattan}')

