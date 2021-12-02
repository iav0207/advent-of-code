#!/usr/bin/python3

import sys

inc = 0
window = []
for line in sys.stdin:
    window.append(int(line))
    inc += 1 if len(window) > 3 and window[-1] > window[0] else 0
    window = window[-3:]
print(inc)

