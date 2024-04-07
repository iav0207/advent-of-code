import sys

wires = sys.stdin.read().splitlines()

def follow(w):
    s = dict()
    x, y, c = 0, 0, 0
    for hop in w.split(','):
        direc, len = hop[0], int(hop[1:])
        dx, dy = {'R': (1, 0), 'L': (-1, 0), 'U': (0, 1), 'D': (0, -1)}[direc]
        for _ in range(len):
            x, y, c = x + dx, y + dy, c + 1
            if (x, y) not in s:
                s[(x, y)] = c
    return s


wires = [follow(it) for it in wires]
cross = set(wires[0].keys()).intersection(set(wires[1].keys()))

print(f'Part 1: {min([x + y for (x, y) in cross])}')
print(f'Part 2: {min([sum(w[it] for w in wires) for it in cross])}')

