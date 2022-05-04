Spec https://adventofcode.com/2021/day/12

Desc Counting ways in which we can get out of the cave system via its numerous tunnels.

## Build and execute at once

Requires `node` and `ts-node` installed.

```bash
make part=1 < input.txt # for part 1
make part=2 < input.txt # for part 2
```

## Compile and execute separately

Requires `node` and `tsc` installed.

Compile
```bash
make build
```

Run
```bash
make part=1 run < input.txt # for part 1
make part=2 run < input.txt # for part 2
```

## Output

```
Part 1: visiting a single small cave twice is not allowed.
There are 3713 paths through the tunnels.
```

```
Part 2: visiting a single small cave twice is allowed.
There are 91292 paths through the tunnels.
```

