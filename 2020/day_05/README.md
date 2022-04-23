Spec https://adventofcode.com/2020/day/5

Binary search of plane seats from boarding passes encoded in binary. Actually, in base-4 alphabet.

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent05.kt -include-runtime -d out/advent_05.jar
make run
java -jar out/advent_05.jar
Max seat ID: Boarding pass BBFFBFFLLR Seat ID=801 row=100 col=1
My seat: Seat ID=597 row=74 col=5
```

