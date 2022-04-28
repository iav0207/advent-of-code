Spec https://adventofcode.com/2020/day/9

Finding an encoding error by summing pairs of ints and brute-forcing sums of contiguous integers in the list.

Execution

```bash
make < input.txt
```

Output

```
make build
rm -rf out
mkdir -p out
kotlinc Advent09.kt -include-runtime -d out/advent_09.jar
make run
java -jar out/advent_09.jar
The first invalid number is 22406676
The min and max of the contiguous set summing up to 22406676 is (931988, 2010399)
Their sum is 2942387
```

