#!/usr/bin/python3

import sys
from itertools import product


start = [int(line.replace('\n', '')[-1]) - 1 for line in sys.stdin.readlines()]
pos = start.copy()
score = [0, 0]
die = 0
rolls = 0


def throw():
    global die
    global rolls
    rolls += 3
    for _ in range(3):
        die += 1
        yield die


while not any(s >= 1_000 for s in score):
    for p in range(2):
        pos[p] = (pos[p] + sum(throw())) % 10
        score[p] += pos[p] + 1
        if score[p] >= 1000:
            break

max_score = max(score)
min_score = min(score)
winner = score.index(max_score)
print(f'\nPart 1: Deterministic dice, playing until scoring 1000')
print(f'Player scores: {score}. The die has been rolled a total of {rolls} times.')
print(f'{min_score} * {rolls} = {min_score * rolls}')

wins = []
target_score = 21
u = []  # score1, score2, pos1, pos2, whose turn it is -> number of universes

for s1 in range(target_score + 2):  # worst array initialization ever omg. at least it works
    u.append([])
    for s2 in range(target_score + 2):
        u[s1].append([])
        for p1 in range(10):
            u[s1][s2].append([[0] * 2 for _ in range(10)])

assert len(u) == target_score + 2
assert len(u[0]) == target_score + 2
assert len(u[0][0]) == 10
assert len(u[0][0][0]) == 10
assert len(u[0][0][0][0]) == 2

u[0][0][start[0]][start[1]][0] = 1  # initially we have only one universe. what a good state to be in

assert u[0][0][start[0]][start[1]][1] == 0


def limit(scr):
    return min(target_score, scr)


for s1, s2, p1, p2 in product(range(target_score), range(target_score), range(10), range(10)):
    assert u[0][0][0][0][0] == 0  # protection from an incorrect array initialization. this fear will persist
    for dice in product([1, 2, 3], [1, 2, 3], [1, 2, 3]):
        advance = sum(dice)
        # player 1
        new_p1 = (p1 + advance) % 10
        new_s1 = limit(s1 + new_p1 + 1)
        u[new_s1][s2][new_p1][p2][1] += u[s1][s2][p1][p2][0]
        # player 2
        new_p2 = (p2 + advance) % 10
        new_s2 = limit(s2 + new_p2 + 1)
        u[s1][new_s2][p1][new_p2][0] += u[s1][s2][p1][p2][1]

wins = [0, 0]
for p1, p2, s in product(range(10), range(10), range(target_score)):
    wins[0] += u[target_score][s][p1][p2][1]
    wins[1] += u[s][target_score][p1][p2][0]

print(f'\nPart 2: Dirac dice')
print(f'Player 1 wins in {wins[0]} universes, player 2 – only in {wins[1]}')


