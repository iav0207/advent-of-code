Spec https://adventofcode.com/2020/day/13

Desc Finding timestamp when bus schedule aligns busses at just the right intervals. Chinese remainder theorem / dynamic programming.

Could not solve part 2 myself, although I knew that Chinese remainder theorem applies here, I could not wrap my head around it.

Found an elegant DP solution which I understand and will be able to reproduce.
The second version though is something I just can't grasp, found it [on Youtube](https://www.youtube.com/watch?v=x40aLK9KjYQ).

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent13.kt -include-runtime -d out/advent_13.jar
make run
java -jar out/advent_13.jar
Can take bus # 419 departing at 1005600, need to wait 5
Part 1: 419 * 5 = 2095
Part 2: t = 598411311431841
```

