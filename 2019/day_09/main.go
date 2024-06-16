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

	for part := 1; part < 3; part++ {
		e := NewEval(code)
		e.Start()
		e.In <- Word(part)
		out := e.CollectOutput()
		if len(out) > 1 {
			fatalf("Wrong answer in part %d, diagnostics: %v\n", out)
		}
		fmt.Printf("Part %d: %d\n", part, out[len(out)-1])
	}
}
