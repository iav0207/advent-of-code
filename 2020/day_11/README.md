Spec https://adventofcode.com/2020/day/11

Desc Running some sort of the Game of Life to find the equilibrium of occupied seats distribution in a waiting room.

Execution

```bash
make
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent11.kt -include-runtime -d out/advent_11.jar
make run part=1 < input.txt
java -jar out/advent_11.jar 1
Part 1: After the chaos stabilizes there are 2470 occupied seats.
make run part=2 < input.txt
java -jar out/advent_11.jar 2
Part 2: After the chaos stabilizes there are 2259 occupied seats.
```

