#!/usr/bin/env python3

import sys
from collections import defaultdict, deque

input = sys.stdin.read().splitlines()


reactions = [
    [[tuple(el.split(" ")) for el in side.split(", ")] for side in line.split(" => ")]
    for line in input
]

# We rely on the fact that there is only one way to obtain an element.
howto = {reac[1][0][1]: reac for reac in reactions}


def part1():
    """
    We store the number of times every element is used in reactions as an ingredient
    to only evaluate the required quantity of this element once we've seen all the uses
    of it. Topological sort.
    Without it we end up with an unnecessary surplus of ingredients: leftovers from
    reactions run repeatedly to obtain the same element.
    So the solution is to traverse the graph in the right order and only execute
    reactions once we know the final amount of the output element that we need.
    """
    wait = defaultdict(int)
    for inp, _ in reactions:
        for _, material in inp:
            wait[material] += 1

    need = defaultdict(int)
    need["FUEL"] = 1

    todo = deque(["FUEL"])
    while todo:
        need_m = todo.popleft()
        if need_m == "ORE":
            break
        need_q = need[need_m]
        reac = howto[need_m]
        reac = scale(reac, need_q)
        for q, m in reac[0]:
            need[m] += int(q)
            wait[m] -= 1
            if not wait[m]:
                todo.append(m)
    return need["ORE"]


def scale(orig_reac, need_qty):
    orig_qty = int(orig_reac[1][0][0])
    if orig_qty >= need_qty:
        return orig_reac
    coef = (need_qty + orig_qty - 1) // orig_qty  # clever way to increase the result by one
                                                  # only when need is not a multiple of orig
    return [[(int(q) * coef, m) for (q, m) in side] for side in orig_reac]


print(f"Part 1: {part1()}")
