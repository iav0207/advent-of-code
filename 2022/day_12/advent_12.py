#!/usr/bin/python3

import sys
from typing import Optional
from dataclasses import dataclass
from queue import PriorityQueue


debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()
field = [list(line) for line in lines]

debug(f'len(field) = {len(field)}')
n, m = len(field), len(field[0])


def find(v):
    for x in range(n):
        for y in range(m):
            if field[x][y] == v:
                yield x, y

start, dest = next(find('S')), next(find('E'))


def manhattan(c1, c2):
    return abs(c1[0] - c2[0]) + abs(c1[1] - c2[1])


@dataclass
class Path:
    head: tuple
    parent: Optional[object]
    length: int

    def neighbours(self):
        xh, yh = self.head
        debug((xh, yh))
        for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            x, y = xh + dx, yh + dy
            if x not in range(n) or y not in range(m):
                continue
            if (x, y) != self.head:
                yield x, y

    def accessible_neighbours(self):
        for neighbour in self.neighbours():
            if self.elevation_of(neighbour) - self.elevation_of(self.head) < 2:
                yield neighbour

    @staticmethod
    def elevation_of(point):
        value = field[point[0]][point[1]]
        return ord({'S': 'a', 'E': 'z'}.get(value, value))

    def create_child(self, nxt):
        return Path(nxt, self, self.length + 1)

    def trace_back(self):
        p = self
        while p.parent:
            yield p.head
            p = p.parent

    def __eq__(self, o):
        return self.length == o.length and self.head == o.head

    def __lt__(self, o):
        if self.length != o.length:
            return self.length < o.length
        return manhattan(self.head, dest) < manhattan(o.head, dest)


def enqueue(path):
    if path.head not in visited:
        pq.put((path.length, path))
        visited.add(path.head)


def dijkstra():
    while not pq.empty():
        p = pq.get()[1]
        if p.head == dest:
            return p
        for nxt in p.accessible_neighbours():
            enqueue(p.create_child(nxt))


pq = PriorityQueue()
visited = set()

p = Path(start, None, 0)
enqueue(p)

p = dijkstra()

debug(list(p.trace_back()))
debug([field[x][y] for x,y in list(p.trace_back())])
print(f'Part 1: {p.length}')

visited.clear()
for s in find('a'):
    debug(f'enqueue {s}')
    enqueue(Path(s, None, 0))

print(f'Part 2: {dijkstra().length}')

