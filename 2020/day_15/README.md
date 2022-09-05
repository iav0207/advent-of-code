Spec https://adventofcode.com/2020/day/15

Desc Elves game: say numbers in rounds depending on when the most recent number was previously spoken.

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent15.kt -include-runtime -d out/advent_15.jar
make run
java -jar out/advent_15.jar
620
110871
```

