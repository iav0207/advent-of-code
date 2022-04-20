#!/usr/bin/python3

import sys

lines = sys.stdin.readlines()

incorrect_valid_count = 0
official_valid_count = 0

for line in lines:
    policy, char, password = line.split(' ')
    char = char[0]
    lo, hi = [int(s) for s in policy.split('-')]

    # part 1
    actual = sum(1 if c == char else 0 for c in password)
    if lo <= actual <= hi:
        incorrect_valid_count += 1

    # part 2
    two_chars = [password[lo - 1], password[hi - 1]]
    if sum(1 if c == char else 0 for c in two_chars) == 1:
        official_valid_count += 1

print(f'{incorrect_valid_count} passwords are valid for the sled rental')
print(f'{official_valid_count} passwords are valid for the Toboggan Corp.')

