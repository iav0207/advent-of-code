#!/usr/bin/python3

import sys

lines = sys.stdin.read().splitlines()

score = 0

wins = {'A Y', 'B Z', 'C X'}

for line in lines:
    opponent, me = line.split(' ')
    if line in wins:
        score += 6
    my_shape_score = 1 + 'XYZ'.index(me)
    opponent_score = 1 + 'ABC'.index(opponent)
    if not (my_shape_score - opponent_score):
        score += 3
    score += my_shape_score

print(f'Part 1 : {score}')

score = 0

for line in lines:
    opponent, todo = line.split(' ')
    mod = 'XYZ'.index(todo) - 1  # lose draw win
    me = 'XYZ'[('ABC'.index(opponent) + mod + 3) % 3]
    my_shape_score = 1 + 'XYZ'.index(me)
    opponent_score = 1 + 'ABC'.index(opponent)
    if not mod:
        score += 3
    if mod == 1:
        score += 6
    score += my_shape_score

print(f'Part 2: {score}')
        
