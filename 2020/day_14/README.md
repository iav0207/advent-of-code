Spec https://adventofcode.com/2020/day/14

Desc Bit operations and masking. It went terrible, with many mistakes along the way. Maybe I should have used char arrays instead of numbers.

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent14.kt -include-runtime -d out/advent_14.jar
make run
java -jar out/advent_14.jar
Part 1: 10050490168421
Part 2: 2173858456958
```

