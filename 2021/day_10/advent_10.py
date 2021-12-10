#!/usr/bin/python3

import sys
from collections import deque

opening_for = { ']': '[', ')': '(', '}': '{', '>': '<' }
completion_for = {v: k for k, v in opening_for.items()}
err_points = { ']': 57, ')': 3, '}': 1197, '>': 25137 }
completion_rank = ' )]}>'

corrupted = 0
incomplete = 0
stx_err_score = 0
completion_scores = []


def process(line):
    global corrupted
    global stx_err_score
    stack = deque()
    for c in line:
        if c in opening_for:
            opening_pair = opening_for[c]
            if stack.pop() != opening_pair:
                corrupted += 1
                stx_err_score += err_points[c]
                return []
        else:
            stack.append(c)
    return stack


def score_completion(stack):
    stack.reverse()
    completion = ''.join([completion_for[c] for c in stack])
    compl_score = 0
    for c in stack:
        compl_score *= 5
        compl_score += completion_rank.index(completion_for[c])
    completion_scores.append(compl_score)


for line in sys.stdin.read().splitlines():
    stack = process(line)
    if stack:
        incomplete += 1
        score_completion(stack)

mid_compl_score = sorted(completion_scores)[int(len(completion_scores)/2)]

print(f'Processed {corrupted} corrupted and {incomplete} incomplete lines.')
print(f'Syntax error score: {stx_err_score}')
print(f'Completion score: {mid_compl_score}')

