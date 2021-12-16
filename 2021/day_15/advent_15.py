#!/usr/bin/python3

import sys
import json
import heapq
from dataclasses import dataclass, field
from queue import PriorityQueue

lines = [line for line in sys.stdin.read().split('\n') if line]
f = [[int(c) for c in line] for line in lines]


field = []


debug = False


def log(something):
    if not debug:
        return
    if isinstance(something, str) or isinstance(something, int):
        print(something)
    else:
        print(json.dumps(something, indent=2))


for x in range(5*len(f)):
    field.append([])
    for y in range(5*len(f)):
        w = f[x%len(f)][y%len(f)] + (x // len(f)) + (y // len(f))
        if w > 9:
            w -= 9
        field[x].append(w)
    log(''.join([str(it) for it in field[x]]))


log(f'field is {len(field)} x {len(field[0])}')

class Path:
    def __init__(self, x, y, w, parent=None):
        self.x = x
        self.y = y
        self.w = w
        self.parent = parent

    def create_child(self, x, y):
        return Path(x, y, self.w + field[x][y], self)

    def trace_back(self):
        p = self
        while p:
            yield p
            p = p.parent

    def __eq__(self, o):
        return self.w == o.w and self.x == o.x and self.y == o.y

    def __lt__(self, o):
        if self.w != o.w:
            return self.w < o.w
        if self.x != o.x:
            return self.x < o.x
        return self.y < o.y


pq = PriorityQueue()
visited = set()


def enqueue(path):
    if (path.x, path.y) not in visited:
        pq.put((path.w, path))
        visited.add((path.x, path.y))


p = Path(0, 0, 0)
enqueue(p)


def adj(p):
    for x in range(max(0, p.x-1), min(len(field), p.x+2)):
        for y in range(max(0, p.y-1), min(len(field), p.y+2)):
            if (x == p.x) != (y == p.y):
                yield x, y



                

while pq:
    p = pq.get()[1]
    log(f'{p.x} {p.y} {p.w}')
    if p.x == len(field)-1 and p.y == len(field)-1:
        break
    for x, y in adj(p):
        enqueue(p.create_child(x, y))

track = [(it.x, it.y) for it in p.trace_back()]
track.reverse()
log(track)
print(f'Min risk is {p.w}')

