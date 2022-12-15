#!/usr/bin/python3

import sys
import re
import math

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


input_text = sys.stdin.read()

source = 500, 0
all_corners = [tuple(int(digit) for digit in it.split(',')) for it in re.findall(f'\d+,\d+', input_text)]
given_coordinates = set(all_corners).union({source})
xs = {x for x, _ in given_coordinates}
ys = {y for _, y in given_coordinates}
xmin, xmax = min(xs), max(xs)
ymin, ymax = min(ys), max(ys)
debug((xmin, xmax, ymin, ymax))


part2 = False  # part 2 extends the field


def all_coordinates():
    for y in range(ymin, ymax + 3):
        for x in range(xmin - (1 if part2 else 0), xmax + (2 if part2 else 1)):
            yield x, y


free = set(all_coordinates())
rock = set()
sand = set()


def is_in_field(coord):
    if part2:
        return True
    x, y = coord
    return x in range(xmin, xmax + 1) and y in range(ymin, ymax + 1)


def make(coord, material):
    if coord in free:
        free.remove(coord)
    material.add(coord)


for line in input_text.splitlines():
    points = [tuple(int(digit) for digit in it.split(',')) for it in re.findall(r'\d+,\d+', line)]
    for i in range(len(points) - 1):
        (x1, y1), (x2, y2) = points[i], points[i + 1]
        if x1 == x2:
            for y in range(y1, y2, int(math.copysign(1, y2 - y1))):
                make((x1, y), rock)
            make((x1, y1), rock)
            make((x1, y2), rock)
        elif y1 == y2:
            for x in range(x1, x2, int(math.copysign(1, x2 - x1))):
                make((x, y1), rock)
            make((x1, y1), rock)
            make((x2, y1), rock)
        else:
            raise RuntimeError(':(')


def draw_field():
    last_y = 0
    buffer = []
    for coord in all_coordinates():
        if coord[1] != last_y:
            debug(''.join(buffer))
            last_y = coord[1]
            buffer = []
        
        if coord == source:
            buffer.append('+')
        elif coord in rock:
            buffer.append('#')
        elif coord in sand:
            buffer.append('o')
        else:
            buffer.append('.')
    debug(''.join(buffer))


draw_field()


def generate_sand():
    x, y = source
    while True:
        candidates = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
        for candidate in candidates:
            if not is_in_field(candidate):
                return None  # no possible point of rest
            if candidate in free:
                break
        if candidate in free:
            x, y = candidate  # falling further
        else:
            break  # found point of rest
    make((x, y), sand)
    return x, y


def run_simulation():
    while True:
        point_of_rest = generate_sand()
        if not point_of_rest:
            break
        if point_of_rest == source:
            break
        # draw_field()


run_simulation()
draw_field()
print(f'Part 1: {len(sand)} grains of sand were needed spill the sand on the floor')

part2 = True

for x in range(1001):
    make((x, ymax + 2), rock)  # adding floor
    for y in range(0, ymax + 2):
        if (x, y) not in rock and (x, y) not in sand:
            make((x, y), free)  # adding space

run_simulation()
draw_field()

print(f'Part 2: {len(sand)} grains of sand were needed to fill the space up')

