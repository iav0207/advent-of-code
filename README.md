# Advent of Code

Helping Santa deliver all the Christmas gifts by solving coding katas at https://adventofcode.com/

[Jump to How-Tos](#how-to)

## Index

- [2020](https://adventofcode.com/2020)
  + [Day 01](./2020/day_01)  Python
    * Two- and three-sums.
  + [Day 02](./2020/day_02)  Python
    * Validating passwords using given policies.
  + [Day 03](./2020/day_03)  Python
    * Counting trees along slopes on an infinite field.
  + [Day 04](./2020/day_04)  Kotlin
    * Passport validation
  + [Day 05](./2020/day_05)  Kotlin
    * Binary search of plane seats from boarding passes encoded in binary. Actually, in base-4 alphabet.
  + [Day 06](./2020/day_06)  Kotlin
    * Counting _yes_ answers among groups of people. First counting questions where _anyone_ answered _yes_, then questions where _everyone_ answered _yes_.
  + [Day 07](./2020/day_07)  Kotlin
    * Counting nested colored bags recursively.
  + [Day 08](./2020/day_08)  Kotlin
    * Fixing a program for a simple turing machine to make it eventually halt.
  + [Day 09](./2020/day_09)  Kotlin
    * Finding an encoding error by summing pairs of ints and brute-forcing sums of contiguous integers in the list.
  + [Day 10](./2020/day_10)  Kotlin
    * Counting ways to arrange hundreds of charger adapters. Dynamic programming.
  + [Day 11](./2020/day_11)  Kotlin
    * Running some sort of the Game of Life to find the equilibrium of occupied seats distribution in a waiting room.
  + [Day 12](./2020/day_12)  Kotlin
    * Navigating a ship according to given instructions. Simple vector abstractions.
  + [Day 13](./2020/day_13)  Kotlin
    * Finding timestamp when bus schedule aligns busses at just the right intervals. Chinese remainder theorem / dynamic programming.
  + [Day 14](./2020/day_14)  Kotlin
    * Bit operations and masking. It went terrible, with many mistakes along the way. Maybe I should have used char arrays instead of numbers.
- [2021](https://adventofcode.com/2021)
  + [Day 01](./2021/day_01)  Python
  + [Day 02](./2021/day_02)  Bash
  + [Day 03](./2021/day_03)  C++ Python
  + [Day 04](./2021/day_04)  Perl
  + [Day 05](./2021/day_05)  Fortran
  + [Day 06](./2021/day_06)  Python
  + [Day 07](./2021/day_07)  Go Python
  + [Day 08](./2021/day_08)  JavaScript
  + [Day 09](./2021/day_09)  C#
  + [Day 10](./2021/day_10)  C Python
    * Syntax scoring of bracket sequences.
  + [Day 11](./2021/day_11)  Python
    * Counting flashes of bioluminescent Dumbo octopuses.
  + [Day 12](./2021/day_12)  Bash TypeScript
    * Counting ways in which we can get out of the cave system via its numerous tunnels.
  + [Day 13](./2021/day_13)  Java
    * Folding an origami to read the hidden message on the transparent paper.
  + [Day 14](./2021/day_14)  Python
    * Growing polymers and counting elements
  + [Day 15](./2021/day_15)  Python
    * Getting out of the cave with a minimal risk. Solved with Dijkstra, however the optimal solution would be linear.
  + [Day 16](./2021/day_16)  Kotlin
    * Reading an encoded expression from packets of data, then evaluating it. Recursive binary protocol.
  + [Day 17](./2021/day_17)  Kotlin
    * Launching an ocean probe.
  + [Day 18](./2021/day_18)  Kotlin
    * Recursive addition of [snailfish](https://en.wikipedia.org/wiki/Snailfish) numbers.
  + [Day 19](./2021/day_19)  Python
    * Reconstructing a 3D picture of beacons in the ocean using partial readings from misoriented misaligned scanners.
  + [Day 20](./2021/day_20)  Python
    * Fancy infinite image enhancement technology.
  + [Day 21](./2021/day_21)  Python
    * Throwing Dirac dice in a multiverse, watching what happens. Bottom-up DP approach.
  + [Day 22](./2021/day_22)  Python
    * Rebooting a submarine reactor. Calculating immense volumes of enormous cuboids overlapping with each other.
  + Day 23 Not done
  + Day 24 Not done
  + [Day 25](./2021/day_25)  Python
    * Sea Cucumber

## How To

### Generate the index

```bash
make toc
```

### Initialize a day

Only Kotlin for now:
```
make day_kt year=2099 day=99 # will initialize Kotlin solution for day 99 of year 2099
```

