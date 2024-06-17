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

	a := NewArcade(code)
	a.Run()
	fmt.Println(a.Field)
	p1 := a.Field.CountTiles("block")

	code[0] = Word(2)
	a = NewArcade(code)
	a.Run()

	fmt.Println()
	fmt.Printf("Part 1: %d\n", p1)
	fmt.Printf("Part 2: %d\n", a.Score)
}
