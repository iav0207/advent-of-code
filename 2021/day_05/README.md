Spec https://adventofcode.com/2021/day/5

Compile (macOS)
```bash
mkdir bin
gfortran ADVENT_05.F -o bin/advent_05
```
Execute
```bash
sed 's/\ -\>\ / /g' input.txt | sed 's/,/\ /g' | bin/advent_05
```
Output for part 1
```
        7085
```
Output for part 2
```
       20271
```


