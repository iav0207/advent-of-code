# Advent of Code

Helping Santa deliver all the Christmas gifts by solving coding katas at https://adventofcode.com/

## Index

- [2021](https://adventofcode.com/2021)
  + [Day 01](./2021/day_01)
  + [Day 02](./2021/day_02)
  + [Day 03](./2021/day_03)
  + [Day 04](./2021/day_04)
  + [Day 05](./2021/day_05)
  + [Day 06](./2021/day_06)


### How to generate the index

```bash
echo 20* | while read year; do echo "- [$year](https://adventofcode.com/$year)"; for folder in $(ls $year | grep day); do day="${folder##*\_}"; echo "  + [Day $day](./$year/day_$day)"; done; done
```

