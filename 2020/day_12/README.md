Spec https://adventofcode.com/2020/day/12

Desc Navigating a ship according to given instructions. Simple vector abstractions.

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent12.kt -include-runtime -d out/advent_12.jar
make run
java -jar out/advent_12.jar
Part 1: Manhattan distance of the ship is 879
Part 2: Manhattan distance of the ship is 18107
```

