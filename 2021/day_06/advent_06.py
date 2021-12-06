#!/usr/bin/python3

import sys
from collections import deque

n = int(sys.argv[1] or 80)
t = deque([0]*9)

for timer in sys.stdin.read().split(','):
    t[int(timer)] += 1

for i in range(n):
    t[7] += t[0]
    t.rotate(-1)

print(' '.join([str(timer) for timer in t]))
print(sum(t))

