Spec https://adventofcode.com/2019/day/16

Desc Didn't crack the idea of part 2, ended up peeking at what others on the
Internet have done.

Things one is supposed to notice in part 2:

1. The offset is very far into the input signal, at the very tail.
1. In that part of the array most of the elements are multiplied by zero in each
   row. The rest are multiplied by one.
1. You can work out the results going backwards, on each row the result is just
   the cumulative sum modulo 10. This is quite fast to compute, do it 100 times.

Execution

```bash
make < input.txt
```

Output

```
Part 1: 50053207
Part 2: 32749588
```

