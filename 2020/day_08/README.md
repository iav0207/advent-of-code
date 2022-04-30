Spec https://adventofcode.com/2020/day/8

Desc Fixing a program for a simple turing machine to make it eventually halt.

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent08.kt -include-runtime -d out/advent_08.jar
make run
java -jar out/advent_08.jar
acc before looping = 1501
acc after the corrected program terminates = 509
```

