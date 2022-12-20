#!/usr/bin/python3

import sys
import re
from itertools import product
from dataclasses import dataclass
from typing import Optional
from queue import PriorityQueue

debug_mode = '-d' in sys.argv
part2 = '2' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()
inputs = [re.findall(r'[A-Z]{2}|\d+', line) for line in lines]
flow_rates = {it[0]: int(it[1]) for it in inputs}
tunnels = {it[0]: it[2:] for it in inputs}

non_zero_capa_valves = set(k for k, v in flow_rates.items() if v > 0)

debug(flow_rates)
debug(tunnels)


@dataclass(eq=False, frozen=False)
class Path:
    head: tuple
    valves_open: set
    parent: Optional[object] = None  # keeping a ref to the parent is here only for debugging, not essential for the algorithm
    released: int = 0
    minutes_elapsed: int = 0
    elephant: Optional[tuple] = None

    def create_children(self):
        if self.valves_open == non_zero_capa_valves:
            yield self
            return
        head_options = [self.head] + tunnels[self.head]
        ele_options = [None] if not part2 else [self.elephant] + tunnels[self.elephant]
        for next_head, next_elephant in product(head_options, ele_options):
            yield self.create_child(next_head, next_elephant)

    def create_child(self, head, elephant=None):
        to_open = set()
        if head == self.head and flow_rates[head] > 0:
            to_open.add(head)
        if part2 and elephant == self.elephant and flow_rates[elephant] > 0:
            to_open.add(elephant)
        return Path(head=head,
                    valves_open=to_open.union(self.valves_open),
                    parent=self,
                    released=self.released,
                    minutes_elapsed=self.minutes_elapsed,
                    elephant=elephant)

    def create_child_with_open_valves(self, *valves):
        to_open = set(v for v in valves if flow_rates[v] > 0)
        return Path(head=self.head,
                    valves_open=to_open.union(self.valves_open),
                    parent=self,
                    released=self.released,
                    minutes_elapsed=self.minutes_elapsed,
                    elephant=self.elephant)

    def release_pressure(self):
        pssh = sum(flow_rates[v] for v in self.valves_open)
        self.released += pssh

    def trace_back(self):
        p = self
        while p:
            yield p.head
            p = p.parent

    def prio(self):
        return -self.released

    def __lt__(self, o):
        return self.prio() < o.prio()


pq = PriorityQueue()
pq.put((0, Path('AA', set(), elephant='AA' if part2 else None)))

# generosity == inverse greediness of the algorithm
# how big the taken sample on every generation can be
# the less the faster, but we can miss out on some winning paths
generosity = 100000 if part2 else 10000

for i in range(25 if part2 else 29):
    debug(f'{i:>2}. qsize: {pq.qsize()}')
    nextgen = PriorityQueue()
    j = 0
    while j < generosity and not pq.empty():
        j += 1
        path = pq.get()[1]
        paths = list(path.create_children())
        for p in paths:
            p.minutes_elapsed = i + 1
            p.release_pressure()
            nextgen.put((p.prio(), p))
    pq = nextgen

best = pq.get()[1]
possible_heads = {best.head}
while not pq.empty():
    other = pq.get()[1]
    if other.released == best.released:
        possible_heads.add(other.head)

print(f'Best: {best.released}, head: {best.head}, vopen: {best.valves_open}, elapsed {best.minutes_elapsed}')
debug(f'Trace: {list(reversed(list(best.trace_back())))}')
debug(f'Other heads possible: {possible_heads}')

if False:  # emulating the optimal path as is given in the example
    actions = [
        'move to valve DD',
        'open valve DD',
        'move to valve CC',
        'move to valve BB',
        'open valve BB',
        'move to valve AA',
        'move to valve II',
        'move to valve JJ',
        'open valve JJ',
        'move to valve II',
        'move to valve AA',
        'move to valve DD',
        'move to valve EE',
        'move to valve FF',
        'move to valve GG',
        'move to valve HH',
        'open valve HH',
        'move to valve GG',
        'move to valve FF',
        'move to valve EE',
        'open valve EE',
        'move to valve DD',
        'move to valve CC',
        'open valve CC',
    ]
    
    p = Path('AA', set())
    
    for i in range(30):
        debug(f'{i:<2}. head at {p.head}, released {p.released}, open: {p.valves_open}')
        if i < len(actions):
            a = actions[i]
            if 'move' in a:
                v = a.split()[-1]
                p = p.create_child(v)
            else:
                p = p. create_child_with_open_valves(p.head)
        p.minutes_elapsed = i + 1
        p.release_pressure()

