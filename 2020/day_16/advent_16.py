#!/usr/bin/python3

import sys
from dataclasses import dataclass
from functools import reduce

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


rule_strings, your_ticket, nearby_tickets = sys.stdin.read().split('\n\n')

rule_strings = rule_strings.splitlines()


def parse_ticket(spec):
    return [int(val) for val in spec.split(',')]


your_ticket = parse_ticket(your_ticket.splitlines()[1])
nearby_tickets = [parse_ticket(it) for it in nearby_tickets.splitlines()[1:]]


debug('\n'.join(rule_strings))
debug('\n'.join(str(it) for it in your_ticket))
for t in nearby_tickets:
    debug(t)

@dataclass
class Rule:
    name: str
    range_one: range
    range_two: range


rules = []
for rule_str in rule_strings:
    name, ranges_str = rule_str.split(':')
    ranges = [r.strip() for r in ranges_str.split('or')]
    range_one_strs = ranges[0].split('-')
    range_one = range(int(range_one_strs[0]), int(range_one_strs[1]) + 1)
    range_two_strs = ranges[1].split('-')
    range_two = range(int(range_two_strs[0]), int(range_two_strs[1]) + 1)
    rules.append(Rule(name, range_one, range_two))


def satisfies(value, rule):
    return value in rule.range_one or value in rule.range_two


error_rate = 0

valid_tickets = []

for ticket in nearby_tickets:
    error_increment = sum(val if all(not satisfies(val, rule) for rule in rules) else 0 for val in ticket)
    error_rate += error_increment
    if not error_increment:
        valid_tickets.append(ticket)

print(f'Error rate is {error_rate}')

fields = ['unknown' for _ in rules]

while 'unknown' in fields:
    changed = False
    debug(f'fields={fields}')
    for ir, rule in enumerate(rules):
        if rule.name in fields:
            continue
        valid_column_nums = []
        for j in range(len(fields)):
            if fields[j] != 'unknown':
                continue
            if all(satisfies(ticket[j], rule) for ticket in valid_tickets):
                valid_column_nums.append(j)
        debug(f'Rule {ir}: valid_column_nums={valid_column_nums}')
        if len(valid_column_nums) == 1:
            j = valid_column_nums[0]
            changed = True
            fields[j] = rule.name
            debug(f'Field {j} is {rule.name}')
    if not changed:
        break

result = reduce(lambda a, b: a * b, [val if 'departure' in fields[j] else 1 for j, val in enumerate(your_ticket)])
print(f'Departure score of my ticket: {result}')

