Spec https://adventofcode.com/2021/day/18

Desc Recursive addition of [snailfish](https://en.wikipedia.org/wiki/Snailfish) numbers.

Implemented avoiding binary trees (because idk). Using trees would be much more straightforward
for magnitude evaluation, I had to come up with a way to fold up the numbers in the tree
from a flat structure – a preorder traversal. At some point I regret not using trees from the beginning,
but to be fair, I guess it was simpler to write _explode_ and _split_ operations on the flat rows.

Execution

```bash
make < input.txt
```

Output

```
Magnitude of the addition result is 4120
Maximum possible magnitude of a pair addition is 4725
```
