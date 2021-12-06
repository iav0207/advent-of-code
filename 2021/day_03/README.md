## Spec

https://adventofcode.com/2021/day/3

## Part 1

Given input data stored in `input.txt`
Compile
```bash
mkdir bin
g++ advent_03_01.cpp -o bin/advent_03_01 --std=c++11
```
and execute
```bash
awk '{$1=$1} 1' FS=  input.txt | rs -Tng0 | bin/advent_03_01 # passing transposed input to the c++ program
```

Output is going to be like
```
499 : 501 = 0
489 : 511 = 0
498 : 502 = 0
490 : 510 = 0
502 : 498 = 1
484 : 516 = 0
515 : 485 = 1
508 : 492 = 1
502 : 498 = 1
518 : 482 = 1
517 : 483 = 1
487 : 513 = 0
mask    00000000000000000000111111111111
gamma   00000000000000000000000010111110 = 190
epsilon 00000000000000000000111101000001 = 3905
190 * 3905 = 741950
```

## Part 2

How to execute
```bash
./advent_2021_03_02.py < input.txt
```

