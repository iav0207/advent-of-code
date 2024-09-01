Spec https://adventofcode.com/2020/day/20

Desc Building a 2D jigsaw puzzle by rotating and flipping and matching tiles.

It took me quite some time to solve the second part. While my initial choice
of plane orientation model was rather handy for enumerating all
possible configurations of a single plane, it turned out to be unfortunately
confusing when I needed to assemble the whole picture thus needing to adjust
piece orientations in the absolute coordinates.

As usual, I introduced many thin abstractions, so it was easy to deduce complex
operations from the more basic ones, but it took a lot of code.

I'm an rather unhappy with how messy the code looks.

Key learnings:

1.  Plane rotation formula, it'd be good to remember:
    ```kotlin
    fun Vec.rotate(side: Int = 1) = Vec(side - 1 - j, i)
    ```
1.  It is rather intuitive to represent plane orientation with two vectors:
    an index and a thumb.
1.  To adjust a vector space (could be a single vector, or some other plane) to
    the orientation of another (call it template), one needs to apply a series of transformations
    that would lead from some default orientation to the template.
    An optimal set of possible transformations is:

    1.  Rotation in one direction.
    1.  Horizontal flip.
    1.  Vertical flip.

    Note. I initially implemented `flip` as always flipping the thumb over the
    index finger. And while this is fine to seek orientations for pairwise
    matching, it does not work well when you need to lay out the pieces on a
    single plane and you need absolute coordinates. The model of serializing
    any orientation into a series of transformations listed above, and then
    applying them to the other object worked more reliably.

Execution

```bash
make < input.txt
```

Output

```
Part 1: 45079100979683
Part 2: 1946
```

