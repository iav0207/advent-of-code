#!/usr/bin/python3

import sys
from collections import deque

n = int(sys.argv[1] or 80)
t = deque([0]*9)

for timer in sys.stdin.read().split(','):
    t[int(timer)] += 1

for i in range(n):
    t.rotate(-1)
    t[6] += t[8]

print(' '.join([str(it) for it in t]))
print(sum(t))

