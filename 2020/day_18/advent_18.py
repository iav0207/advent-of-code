#!/usr/bin/python3

import sys
from collections import deque
from dataclasses import dataclass
from functools import reduce

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()


results = []
result = 0


def eval(le, op, ri):
    return le + ri if op == '+' else le * ri


for line in lines:
    debug(f'new line: {line}')
    stack = deque()  # stack of (line_position, value_so_far: 0 by default, operation: + by default)
    stack.append((-1, 0, '+'))
    op = '+'
    for i, c in enumerate(line):
        if c == ' ':
            continue
        debug(stack)
        if c == '(':
            stack.append((i, 0, '+'))
            continue
        elif c == ')':
            closed = stack.pop()
            ctx = stack.pop()
            stack.append((i, eval(ctx[1], ctx[2], closed[1]), '+'))
            continue
        elif c == '+' or c == '*':
            op = c
            (ii, vv, _) = stack.pop()
            stack.append((ii, vv, op))
            continue
        else:
            _, left, op = stack.pop()
            right = int(c)
            stack.append((i, eval(left, op, right), '+'))
    assert len(stack) == 1
    line_result = stack.pop()[1]
    results.append(line_result)
    result += line_result

debug(results)
print(f'Part 1: {result}')

results = []
result = 0


for line in lines:
    debug(f'new line: {line}')
    stack = deque()  # stack of a list of multiplicators: (line_position, value_so_far: 0 by default, operation: + by default)
    stack.append([(-1, 1, '*')])
    op = '*'
    for i, c in enumerate(line):
        if c == ' ':
            continue
        debug(stack)
        if c == '(':
            stack.append([(i, 1, '*')])
            continue
        elif c == ')':
            closed = stack.pop()
            assert all(op == '*' for _, _, op in closed)
            closed = reduce(lambda le, ri: le * ri, [val for _, val, _ in closed])
            ctx_all = stack.pop()
            ctx = ctx_all[-1]
            if ctx[-1] == '+':
                ctx_all[-1] = (i, eval(ctx[1], ctx[2], closed), '*')
            else:
                ctx_all.append((i, closed, '*'))
            stack.append(ctx_all)
            continue
        elif c == '+' or c == '*':
            op = c
            (ii, vv, _) = stack[-1][-1]
            stack[-1][-1] = (ii, vv, op)
            continue
        else:
            right = int(c)
            if op == '*':
                ii, vv, _ = stack[-1][-1]
                stack[-1][-1] = ii, vv, '*'
                stack[-1].append((i, right, '*'))
            else:
                _, left, op = stack[-1][-1]
                stack[-1][-1] = (i, eval(left, op, right), '*')
    debug(stack)
    assert len(stack) == 1
    assert all(op == '*' for _, _, op in stack[0])
    line_result = reduce(lambda le, ri: le * ri, [val for _, val, _ in stack[0]])
    results.append(line_result)
    result += line_result

debug(results)
print(f'Part 2: {result}')

