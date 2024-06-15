package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	input, err := reader.ReadString('\n')
	fatalIf(err, "read stdin")
	code := asCode(strings.ReplaceAll(input, "\n", ""))

	for _, part := range []struct {
		n        int
		inputVal Word
	}{
		{1, Word(1)}, {2, Word(5)},
	} {
		result := run(code, part.inputVal)
		n := len(result.Output)

		if !result.Halted {
			fatalf("aborted: %v\n", result)
		}
		for i, w := range result.Output {
			if w != 0 && i < n-1 {
				fatalf("test #%d failed: %d\n", i, w)
			}
		}
		if len(result.Output) > 0 {
			fmt.Printf("Part %d: %d\n", part.n, result.Output[n-1])
		} else {
			fmt.Println("Success, no output.")
		}
	}
}
