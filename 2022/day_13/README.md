Spec https://adventofcode.com/2022/day/13

Desc Comparing nested packets of integers, sorting the packets. Recursion, comparators.

Tried to solve in JavaScript, got stuck (the example passed, but the answer given on the input is wrong). Solved from scratch in Python, worked like a charm. Then I debugged the two solutions by analyzing discrepancies in their results and finally fixed the initial JavaScript solution.

Key takeaway: when dealing with comparisons, use comparator function abstraction from the very beginning to implement ternary logic. Using `true`, `false` and `undefined` will get you doubting your design all the time, as `undefined` is not as reliable as an intentional value of `0`. Additionally, using comparator abstraction made the part 2 solution trivial.

Execution

```bash
./advent_13.py < input.txt
```

Output

```
Part 1: 5013
Part 2: 25038
```

