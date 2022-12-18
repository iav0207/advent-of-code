#!/usr/bin/python3

import sys
import re
from itertools import product
import numpy as np

debug_mode = '-d' in sys.argv
part2 = '2' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()
inputs = [re.findall(r'[A-Z]{2}|\d+', line) for line in lines]
names = [it[0] for it in inputs]
flow_rates = [int(it[1]) for it in inputs]
tunnels = [it[2:] for it in inputs]
# flow_rates = {it[0]: int(it[1]) for it in inputs}
# tunnels = {it[0]: it[2:] for it in inputs}
n = len(tunnels)

index = {it[0]: i for i, it in enumerate(inputs)}
index.update({i: it[0] for i, it in enumerate(inputs)})

valve_states = tuple(product(*[{0, fr} for fr in flow_rates]))
valve_states_index = {v: k for k, v in enumerate(valve_states)}
print(f'Valve states count: {len(valve_states)}')
debug(valve_states)

debug(flow_rates)
debug(tunnels)
debug(index)

p = np.full(( # pressure released where
    31,                 # minutes elapsed
    n,                  # head position
    len(valve_states),  # valves state
), -1, dtype=np.int32)

p[0, index['AA'], 0] = 0
debug(p[:2, index['AA']])

total_minutes = 30
last_printed_minute = 0
max_capacity = sum(flow_rates)

running_max = -1

for minute, head, valve_state_code in product(range(total_minutes), range(n), range(len(valve_states))):
    vs = valve_state_code
    if p[minute, head, vs] < 0:
        continue

    if last_printed_minute < minute:
        last_printed_minute = minute
        if minute > 2:
            running_max = max(running_max, p[minute-2, :, :].max())
        print(minute)
    curr_valves = valve_states[vs]
    pressure_released_so_far = p[minute, head, vs]
    pressure_release = sum(curr_valves)  # TODO or new_valves?
    if sum(curr_valves) == max_capacity:
        p[minute + 1, head, vs] = max(p[minute + 1, head, vs], pressure_released_so_far + pressure_release)
        continue
    if p[minute, head, vs] < running_max:
        continue

    for next_head in [head] + [index[it] for it in tunnels[head]]:
        vs = valve_state_code
        if next_head == head and curr_valves[head] == 0 and flow_rates[head] > 0:
            new_valves = list(curr_valves)
            new_valves[head] = flow_rates[head]
            new_valves = tuple(new_valves)
            vs = valve_states_index[new_valves]
        old_next = p[minute + 1, next_head, vs]
        p[minute + 1, next_head, vs] = max(old_next, pressure_released_so_far + pressure_release)


def example():
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
    head, vs = index['AA'], 0
    yield 'start', 0, head, vs
    for i in range(30):
        if i < len(actions):
            a = actions[i]
            if 'move' in a:
                head = index[a.split()[-1]]
            else:
                new_valves = list(valve_states[vs])
                new_valves[head] = flow_rates[head]
                vs = valve_states.index(tuple(new_valves))
        yield a, i + 1, head, vs


if debug_mode:
    for action, minute, head, vs in example():
        debug(action)
        debug(f'{minute:>2}. p {p[minute, head, vs]} head {index[head]} vopen {valve_states[vs]}')

print(f'Part 1: {p[30, :, :].max()}')

