Spec https://adventofcode.com/2022/day/16

Desc Moving through the tunnels and opening valves to release as much pressure as possible within 30 minutes before volcano erupts.

Solved with brute force, trying all possible options. Used greedy approach, limiting the number of candidates on every generation to an arbitrary number. The solution is quite slow, specifically for part 2, I guess there must be a more efficient approach.

Execution

```bash
make    # will execute both parts
```

Output

```
make run part=1 < input.txt
./advent_16.py 1
Best: 1737, head: AR, vopen: {'GL', 'AR', 'AH', 'XN', 'OW', 'VR', 'HZ', 'JF'}, elapsed 29
make run part=2 < input.txt
./advent_16.py 2
Best: 2216, head: TS, vopen: {'GL', 'UP', 'HZ', 'AH', 'AR', 'VR', 'VO', 'JF', 'EJ', 'XN', 'OW'}, elapsed 25
```

