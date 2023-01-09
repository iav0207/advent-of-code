Spec https://adventofcode.com/2022/day/19

Desc Building resource-mining robots optimally to maximize the number of geodes within a certain time. Brute-force + limiting the space of possible states.

[At first](advent_19_0.py) I was implementing a greedy algorithm, but it was not working out well.

After the competition ended I saw how Jonathan Paulson [solved](https://github.com/jonathanpaulson/AdventOfCode/blob/master/2022/19.py) the problem,
and implemented a [similar solution](advent_19.py). The idea of reducing the state space is very interesting, although I have a feeling that there must be a more efficient approach
to this problem. Check linear programming or constraint programming: https://en.wikipedia.org/wiki/Constraint_programming or https://developers.google.com/optimization/cp

Execution

```bash
./advent_19.py < input.txt
```

Output

```
Solving BP 1...
Solving BP 2...

...

Solving BP 29...
Solving BP 30...
==============================
          Part 1: 1725
==============================
Solving BP 1...
Solving BP 2...
Solving BP 3...
==============================
          Part 2: 15510
==============================
```

