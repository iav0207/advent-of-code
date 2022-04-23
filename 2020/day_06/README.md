Spec https://adventofcode.com/2020/day/6

Counting _yes_ answers among groups of people. First counting questions where _anyone_ answered _yes_, then questions where _everyone_ answered _yes_.

Execution

```bash
make < input.txt
```

Output
```
make build
rm -rf out
mkdir -p out
kotlinc Advent06.kt -include-runtime -d out/advent_06.jar
make run
java -jar out/advent_06.jar
The sum of group counts is: any=6885 all=3550
```

