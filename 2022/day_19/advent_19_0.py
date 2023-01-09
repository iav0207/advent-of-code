#!/usr/bin/env python3

import sys
import re
import json
from dataclasses import dataclass
from queue import PriorityQueue

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()

blueprints = []
for line in lines:
    nums = re.findall(r'\d+', line)
    blueprints.append(
        {
            'ore': {
                'ore': int(nums[1]),
            },
            'clay': {
                'ore': int(nums[2]),
            },
            'obsidian': {
                'ore': int(nums[3]),
                'clay': int(nums[4]),
            },
            'geode': {
                'ore': int(nums[5]),
                'obsidian': int(nums[6]),
            },
        }
    )

debug(json.dumps(blueprints, indent=2))


@dataclass(eq=True, frozen=True)
class Resources:
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geode: int = 0

    def __add__(self, other):
        return Resources(self.ore + other.ore, self.clay + other.clay, self.obsidian + other.obsidian, self.geode + other.geode)

    def __sub__(self, other):
        return Resources(self.ore - other.ore, self.clay - other.clay, self.obsidian - other.obsidian, self.geode - other.geode)

    def affordable_robots(self, bpid):
        bp = blueprints[bpid]
        if self.ore >= bp['ore']['ore']:
            yield 'ore', Resources(ore=int(bp['ore']['ore']))
        if self.ore >= bp['clay']['ore']:
            yield 'clay', Resources(ore=int(bp['clay']['ore']))
        if self.ore >= bp['obsidian']['ore'] and self.clay >= bp['obsidian']['clay']:
            yield 'obsidian', Resources(ore=int(bp['obsidian']['ore']), clay=int(bp['obsidian']['clay']))
        if self.ore >= bp['geode']['ore'] and self.obsidian >= bp['geode']['obsidian']:
            yield 'geode', Resources(ore=int(bp['geode']['ore']), obsidian=int(bp['geode']['obsidian']))

    def __lt__(self, o):
        if self.geode != o.geode:
            return self.geode < o.geode
        if self.obsidian != o.obsidian:
            return self.obsidian < o.obsidian
        if self.clay != o.clay:
            return self.clay < o.clay
        if self.ore != o.ore:
            return self.ore < o.ore
        return False


@dataclass(eq=True)
class Robots:
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geode: int = 0

    @staticmethod
    def one_of_type(rtype):
        robot = Robots()
        setattr(robot, rtype, 1)
        return robot

    def __add__(self, other):
        return Robots(self.ore + other.ore, self.clay + other.clay, self.obsidian + other.obsidian, self.geode + other.geode)

    def __lt__(self, o):
        if self.geode != o.geode:
            return self.geode < o.geode
        if self.obsidian != o.obsidian:
            return self.obsidian < o.obsidian
        if self.clay != o.clay:
            return self.clay < o.clay
        if self.ore != o.ore:
            return self.ore < o.ore
        return False


@dataclass(eq=True, frozen=True)
class State:
    bpid: int
    res: Resources = Resources()
    rob: Robots = Robots(ore=1)
    prod: Robots = Robots()  # robots in production
    time: int = 0

    def create_children(self):
        # no-op
        yield self.create_child(self.res + self.rob, self.rob)
        # create robots
        # FIXME building a robot should last 1 minute
        if self.bpid == 0 and self.time == 2:
            debug(f'available robots: {list(self.res.affordable_robots(self.bpid))}')
        for robot_type, res_req in self.res.affordable_robots(self.bpid):
            yield self.create_child(self.res - res_req + self.rob, self.rob + Robots.one_of_type(robot_type))

    def create_child(self, res, rob, start_b_rob=Robots()):
        rob = rob + self.prod  # production complete
        return State(self.bpid, res, rob, start_b_rob, self.time + 1)

    def __lt__(self, o):  # inverse (for min priority queue)
        if self.res != o.res:
            return self.res > o.res
        if self.rob != o.rob:
            return self.rob > o.rob
        if self.time != o.time:
            return self.time > o.time
        return self.bpid > o.bpid


results = [0 for _ in blueprints]
time_limit = 24  # FIXME
generosity = 10000
pq, nextgen = PriorityQueue(), PriorityQueue()

for bpid in range(len(blueprints)):
    pq.put(State(bpid))

for minute in range(time_limit):
    print(f'minute {minute}...')
    c = 0
    for j in range(generosity):
        if minute >= 23:
            debug((j, pq.qsize()))
        if pq.empty():
            break
        s = pq.get()
        debug(f'got {s}')
        for child in s.create_children():
            c += 1
            nextgen.put(child)
    debug(f'put {c} children')
    pq, nextgen = nextgen, pq
    nextgen.queue.clear()

if False:
    while not pq.empty():
        debug(f'q {pq.get()}')
    exit(0)

winner = pq.get()
debug(f'winner: {winner}')
quality_level = (winner.bpid + 1) * winner.res.geode

print(f'Part 1: {quality_level}')

