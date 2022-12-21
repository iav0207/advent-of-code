#!/usr/bin/env python3

import sys
import re

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()
lines = [re.findall(r'\w+|[+\-/*]+', line) for line in lines]
monkeys = {it[0]: tuple(it[1:]) for it in lines}

debug(monkeys)

ops = {'+': lambda a, b: a + b, '-': lambda a, b: a - b, '*': lambda a, b: a * b, '/': lambda a, b: int(a / b)}

def eval(monkey):
    expr = monkeys[monkey]
    if len(expr) == 1:
        return int(expr[0])
    else:
        a, op, b = expr
        return ops[op](eval(a), eval(b))

print(f'Part 1: {eval("root")}')

mem = dict()
history = []

target = None
human = 0
corrected_number = None  # 0 for left, 1 for right
diff = None


def root(h):  # left and right terms at root monkey level with given humn value
    monkeys['humn'] = h,
    le, _, ri = monkeys['root']
    return eval(le), eval(ri)


# assuming that diff(human) is monotonic,
# using descent via linear regression (hope it's the right term)

while True:
    history.append(human)
    lev, riv = root(human)
    mem[human] = (lev, riv)
    if lev == riv:  # bingo!
        print(f'{lev} == {riv} when you yell {human}')
        print(f'Part 2: {human}')
        break
    debug(f'{lev} != {riv}')
    if len(mem) == 5:  # just once: indentify which number is the reference (target) and which is the fitted value
        old, new = mem[0], mem[4]
        target = old[0] if old[0] == new[0] else old[1]
        corrected_number = 1 if target == old[0] else 0
        diff = target - mem[human][corrected_number]
        debug(f'target is {target}, diff is {diff}')
    if target is None:
        human += 1
        continue
    diff = target - mem[human][corrected_number]
    prediff = target - mem[history[-2]][corrected_number]
    debug(f'diff {diff}')
    debug(f'prediff {prediff}')
    # now the descent happens
    # the conditional branches below are a result not of focused edge case analysis,
    # but a rapid figuring out what leads to the target value
    # while debugging against the clock
    sign = 1 if diff > 0 else -1
    if prediff - diff == 0:  # avoiding division by zero
        inc = 100
    elif (prediff > 0) != (diff > 0):
        debug(f'human={human} inc={inc} / 2')
        inc = - int(inc / 2)
    elif abs(diff) < 100000:
        inc = - sign * int(inc / 2)
        debug(f'human={human} inc={inc}')
    else:
        inc = sign * int(diff / (diff - prediff))
    human += inc

# 3451534022349 is not the right answer


def test(h):
    lev, riv = root(h)
    return lev - riv


hmin = next(h for h in range(human, human - 100, -1) if test(h) != 0) + 1
hmax = next(h for h in range(human, human + 100,  1) if test(h) != 0) - 1

print(f'Part 2: actually, the answer is in [{hmin}, {hmax}]')

