Spec https://adventofcode.com/2020/day/4

Passport validation

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent04.kt -include-runtime -d out/advent_04.jar
make run
java -jar out/advent_04.jar
264 passports are complete
224 passports are valid
```

