#!/usr/bin/python3

import sys

inc = 0
prev = None
for line in sys.stdin:
    line_int = int(line)
    inc += 1 if prev and line_int > prev else 0
    prev = line_int
print(inc)

