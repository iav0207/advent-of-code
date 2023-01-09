#!/usr/bin/env python3

import json
import re
import sys
from collections import deque
from functools import reduce

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()

blueprints = [
    [int(it) for it in re.findall(r'\d+', line)]
    for line in lines
]

debug(json.dumps(blueprints, indent=2))

# state: res, rob, time
# res, rob: ore, clay, obsidian, geode
# costs: ore, ore, (ore, clay), (ore, obsidian)


def add(vec1, vec2):
    return tuple(a + b for a, b in zip(vec1, vec2))

def mins(vec1, vec2):
    return tuple(min(a, b) for a, b in zip(vec1, vec2))


def max_geodes(costs, time):
    c0, c1, c2, c3 = costs
    # max resource amounts we can spend per minute
    max_spends = max((c0, c1, c2[0], c3[0])), c2[1], c3[1], 999999999
    debug(f'max spends {max_spends}')
    state = (0, 0, 0, 0), (1, 0, 0, 0), time
    todo = deque([state])
    seen = set()
    counter = 0
    ret = 0
    t_ret = dict()

    def optimize(s):
        res, rob, t = s
        # we don't need to generate resources at a rate exceeding max spending rate
        rob = mins(rob, max_spends)
        # we don't need resource supply exceeding the amount of resources
        # we can spend until time is up minus the amount of resources
        # that the present robots will generate in the remaining time
        res = list(res)
        for k in range(3):  # for each resource type
            can_ever_spend = t * max_spends[k]
            will_generate_at_least = rob[k] * (t - 1)
            may_need_supply = can_ever_spend - will_generate_at_least
            res[k] = min(res[k], may_need_supply)
        return tuple(res), rob, t

    def enqueue(s):
        todo.append(optimize(s))

    while todo:
        state = todo.popleft()
        if state in seen:
            continue
        seen.add(state)
        # debug(state)
        if counter % 10000 == 0:
            debug(counter)
        counter += 1
        res, rob, t = state

        if res[3] > ret:
            ret = res[3]
            t_ret[t] = state

        if time - t == 19 and rob == (1, 4, 2, 1) and res == (3, 21, 5, 1):
            debug(f'FOUND {state}')

        # termination
        if t == 0:
            continue
        
        # creating successor states
        nres = add(res, rob)
        enqueue((nres, rob, t-1))
        if res[0] >= c0:
            enqueue((
                add(nres, (-c0, 0, 0, 0)),
                add(rob, (1, 0, 0, 0)),
                t - 1
            ))
        if res[0] >= c1:
            enqueue((
                add(nres, (-c1, 0, 0, 0)),
                add(rob, (0, 1, 0, 0)),
                t - 1
            ))
        if res[0] >= c2[0] and res[1] >= c2[1]:
            enqueue((
                add(nres, (-c2[0], -c2[1], 0, 0)),
                add(rob, (0, 0, 1, 0)),
                t - 1
            ))
        if res[0] >= c3[0] and res[2] >= c3[1]:
            enqueue((
                add(nres, (-c3[0], 0, -c3[1], 0)),
                add(rob, (0, 0, 0, 1)),
                t - 1
            ))

    for k, v in sorted(t_ret.items()):
        debug(f'{time-k}: {v}')
    return ret


solved = dict()

for b in blueprints:
    b_id = b[0]
    print(f'Solving blueprint {b_id}...')
    solved[b_id] = max_geodes((b[1], b[2], (b[3], b[4]), (b[5], b[6])), 24)

winner = max(solved.items(), key=lambda kv: solved[kv[0]])

debug(f'winner: {winner}')
quality_level = winner[0] * winner[1]

total_ql = sum(k*v for k, v in solved.items())

print('=' * 30)
print((' ' * 10) + f'Part 1: {total_ql}')
print('=' * 30)

solved = dict()

for b in blueprints[:3]:
    b_id = b[0]
    print(f'Solving blueprint {b_id}...')
    solved[b_id] = max_geodes((b[1], b[2], (b[3], b[4]), (b[5], b[6])), 32)

answer = reduce(lambda acc, it: acc * it, solved.values())

print('=' * 30)
print((' ' * 10) + f'Part 2: {answer}')
print('=' * 30)

