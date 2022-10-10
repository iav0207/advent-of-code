#!/usr/bin/python3

import sys
from dataclasses import dataclass

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()

print('\n'.join(lines))


@dataclass
class Cube:
    x: int
    y: int
    z: int

    def neighbors(self):
        for x in range(self.x - 1, self.x + 2):
            for y in range(self.y - 1, self.y + 2):
                for z in range(self.z - 1, self.z + 2):
                    if x == self.x and y == self.y and z == self.z:
                        continue
                    yield Cube(x, y, z)

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __eq__(self, o):
        return self.x == o.x and self.y == o.y and self.z == o.z


@dataclass
class State:
    active: set  # of cubes

    def evaluate_new(self):
        old, new = self, State(set())
        todo = set()
        for c in old.active:
            for cc in c.neighbors():
                todo.add(cc)

        def count_active_neighbors(cc):
            return sum(1 if n in old.active else 0 for n in cc.neighbors())

        for c in todo:
            active_neighbor_count = count_active_neighbors(c)
            if c in old.active and active_neighbor_count in range(2, 4):
                new.active.add(c)  # remains active
            elif c not in old.active and active_neighbor_count == 3:
                new.active.add(c)  # becomes active
        return new


current = State(set())

# init state

for x, line in enumerate(lines):
    for y, c in enumerate(line):
        if c == '#':
            current.active.add(Cube(x, y, 0))

for step in range(6):
    current = current.evaluate_new()

print(f'{len(current.active)} cubes are active after 6 steps')

