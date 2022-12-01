#!/usr/bin/python3

import sys

elves = sys.stdin.read().split('\n\n')
elf_totals = [sum(int(it) for it in elf.splitlines()) for elf in elves]
print(f'Max: {max(elf_totals)}')
print(f'Max 3 sum: {sum(sorted(elf_totals)[-3:])}')

