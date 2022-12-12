Spec https://adventofcode.com/2022/day/11

Desc Monkey business: throwing big integer items between monkeys. Modulo trickery, Chinese remainder theorem.

The idea of worryLimit in part 2 is that we don't need to know the exact item values,
we only need to route items to correct monkeys and count item inspections
by each monkey.
The only criterion for proper routing is integer division by a number,
one per monkey.
Therefore, any value can be taken modulo the product of all monkey divisors
to limit monkey worry levels.

Execution

```bash
make part=1 < input.txt
make part=2 < input.txt
```

Output

```
Part 1: Monkey business level is 78678
Part 2: Monkey business level is 15333249714
```

