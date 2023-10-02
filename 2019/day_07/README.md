Spec https://adventofcode.com/2019/day/7

Desc Finding the best circuit of amplifiers, each amplifier is an intcode computer. Used Golang channels and goroutines. Permutations in Go.

Execution

```bash
go run advent_07.go < input.txt
```

Output

```
Part 1: highest signal is 75228, sequence [0 3 4 2 1]
Part 2: highest signal is 79846026, sequence [6 7 9 5 8]
```

## Notes

Iterating over all permutations of amp phase configurations was initially implemented as

1. Take all combinations.
1. Filter out those with repeating phase values.

But then I decided to rework that part of the code to actually compute permutations. I expected
no significant performance gain for this problem and did it just for fun.

Go is pain, but at least I learned something having spent ~ two hours only to find a bug in the
way I concatenated slices, like so: `next := append(pool[:i], pool[i+1:]...)`. Golang
fanatically does not provide a safe way of slices concatenation, the idea is to only use
`append(a, b...)`, but then it turns out that this operation overwrites the rest of the array
underlying `a`. In fact, there is a [whole bunch](https://medium.com/@nsspathirana/common-mistakes-with-go-slices-95f2e9b362a9)
of ways to shoot yourself in the foot with this basic data structure in a language that is meant
to be _obvious_. Well.

I'll end my rant here by saying that what in Python would have taken literally two minutes, and this code:

```python
def permute(n):
    yield from permute_rec(list(range(n)))


def permute_rec(pool):
    for i in range(len(pool)):
        for sub in permute_rec(pool[:i] + pool[i+1:]):
            yield [pool[i]] + sub
    if not pool:
        yield []


for p in permute(3):
    print(p)

```

...looks like this in Go:

```go
func permute(n int) <-chan []int {
	pool := make([]int, n)
	for i := 0; i < n; i++ {
		pool[i] = i
	}
	ch := make(chan []int)
	go func() {
		defer close(ch)
		for permutation := range permuteRec(pool) {
			ch <- permutation
		}
	}()
	return ch
}

func permuteRec(pool []int) <-chan []int {
	ch := make(chan []int)
	go func() {
		defer close(ch)
		for pi, p := range pool {
			next := append([]int{}, pool[:pi]...)
			next = append(next, pool[pi+1:]...)
			for sub := range permuteRec(next) {
				ch <- append([]int{p}, sub...)
			}
		}
		if len(pool) == 0 {
			ch <- nil
		}
	}()
	return ch
}

import "fmt"

func main() {
    for p := range permute(3) {
        fmt.Println(p)
    }
}
```

