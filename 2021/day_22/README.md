Spec https://adventofcode.com/2021/day/22

Rebooting a submarine reactor.

Calculating immense volumes of enormous cubes overlapping with each other.

Learned

1. coordinate compression
2. multiprocessing in Python and sharing numpy arrays fairly between the workers
5. there is no pypy3 for arm64 architecture (M1 chip)
3. that 3 dimensions is too many for me
4. off by one errors (oh I've had them enough) turn into off by over 9000 all over the place

Execution

```bash
./advent_22.py < input.txt    # to run on one cpu core
./advent_22.py 6 < input.txt  # to run on 6 cores
```

Output

```
[[1414 2285 3535 ... 4158    1    1]
 [ 856   19  921 ...    1    1    0]
 [ 531  616 1867 ...    0    0    0]]
Amount of work: 1898928560 3d elements.
Sharing the job: spawning 4 workers
[]
[]
[]
[]
NEW WORKER: shape (311, 1240, 1234) start i =     0. To do:  475879760
Done 0.21%
NEW WORKER: shape (310, 1240, 1234) start i =   311. To do:  474349600
Done 0.42%
NEW WORKER: shape (310, 1240, 1234) start i =   621. To do:  474349600
Done 0.63%
NEW WORKER: shape (310, 1240, 1234) start i =   931. To do:  474349600
Done 0.84%
Done 1.1%
Done 1.3%

...

1387966280636636 cubes are lit.
```
