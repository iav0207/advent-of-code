#!/usr/bin/python3

import sys

debug_mode = '-d' in sys.argv


def debug(obj):
    if debug_mode:
        print(str(obj))


lines = sys.stdin.read().splitlines()

print('\n'.join(lines))

