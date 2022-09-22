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
    invalid_values = [val for val in ticket if all(not satisfies(val, rule) for rule in rules)]
    if invalid_values:
        error_rate += sum(invalid_values)
    else:
        valid_tickets.append(ticket)

print(f'Error rate is {error_rate}')

def candidate_column_nums(rule):
    return [j for j in range(len(rules)) if all(satisfies(ticket[j], rule) for ticket in valid_tickets)]

fields = {r.name: {'col': None, 'candidates': candidate_column_nums(r)} for r in rules}

def find_rule_w_singe_candidate():
    return next(([k, v['candidates'][0]] for k, v in fields.items() if len(v['candidates']) == 1))

for _ in range(len(fields)):
    # debug(f'fields={fields}')
    rule_name, col = find_rule_w_singe_candidate()
    debug(f'field {col} is {rule_name}')
    fields[rule_name]['col'] = col
    for v in fields.values():
        if col in v['candidates']:
            v['candidates'].remove(col)

result = reduce(lambda a, b: a * b, [your_ticket[v['col']] for k, v in fields.items() if 'departure' in k])
print(f'Departure score of my ticket: {result}')

