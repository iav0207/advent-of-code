#!/usr/bin/python3

import sys
import json
from functools import cmp_to_key


debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(obj)


pairs = sys.stdin.read().split('\n\n')
pairs = [tuple(json.loads(each) for each in it.splitlines()) for it in pairs if it]

debug(f'Read {len(pairs)} pairs')


def wrap_if_int(o):
    return [o] if type(o) == int else o


def compare(a, b):
    debug(f'a={a}, b={b}')

    if type(a) == int and type(b) == int:
        if a == b:
            return 0
        return 1 if a < b else -1

    if type(a) == int or type(b) == int:
        return compare(wrap_if_int(a), wrap_if_int(b))

    for le, ri in zip(a, b):
        if type(le) == int and type(ri) == int:
            cmp = compare(le, ri)
            if cmp != 0:
                return cmp
        elif type(le) != int and type(ri) != int:
            for lei, rii in zip(le, ri):
                cmp = compare(lei, rii)
                if cmp != 0:
                    return cmp
            cmp = compare(len(le), len(ri))
            if cmp != 0:
                return cmp
        else:
            cmp = compare(wrap_if_int(le), wrap_if_int(ri))
            if cmp != 0:
                return cmp

    return compare(len(a), len(b))


part1answer = sum(idx + 1 for idx, (pa, pb) in enumerate(pairs) if compare(pa, pb) == 1)
print(f'Part 1: {part1answer}')

dividers = [[2]], [[6]]

items = [item for pair in pairs for item in pair]
sorted_items = sorted(items + list(dividers), key=cmp_to_key(compare), reverse=True)
debug(sorted_items)

indices = [sorted_items.index(div) + 1 for div in dividers]
decoder_key = indices[0] * indices[1]
debug(indices)

print(f'Part 2: {decoder_key}')

