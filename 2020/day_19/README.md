Spec https://adventofcode.com/2020/day/19

Desc Matching messages against recursive rules (finite state automaton).

Execution

```bash
make build
make part1 < input.txt
```

Output

```
Part 1: 224
```

Part 2 is not solved.

It was an interesting task.

First I implemented a plain recursion: every rule
was a matcher that advances along the given string by a set / sequence of integers,
including all possible advances of the included sub-rules.
Using that approach I solved part one, but the whole thing breaks (I believe)
once you introduce loops.

So then I re-implemented it all from scratch
using finite automata abstraction: the given top-level rule (`0` in the assignment)
instantiates the non-deterministic finite automaton, which constructs every
nested rule as an automaton of its own.

To account for loops (one cannot just build the whole automaton, because they'd
run into an infinite recursion) I had to make the automata lazily instantiated,
so I introduced proxies. This worked well in that it did not break at part two,
but the answer that my solution gives is incorrect and needs debugging.

In general, I think that although building NFAs is fun, the approach is too complex
for a competitive programming problem.
