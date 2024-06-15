package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strings"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	input, err := reader.ReadString('\n')
	fatalIf(err, "read stdin")
	original := asCode(strings.ReplaceAll(input, "\n", ""))

	code := slices.Clone(original)
	code[1] = 12
	code[2] = 2
	run(code)
	debugln(formatCode(code))
	fmt.Printf("Part 1: %d\n", code[0])

	var search int64 = 19690720
	for noun := 0; noun < 100; noun++ {
		for verb := 0; verb < 100; verb++ {
			code := slices.Clone(original)
			code[1] = int64(noun)
			code[2] = int64(verb)
			run(code)
			if code[0] == search {
				fmt.Printf("Part 2: %d\n", 100*noun+verb)
				return
			}
		}
	}
}
