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

	d := NewDroid(code)
	d.Eval.Start()
	d.Explore()

	d.BFSFrom(d.Position)
	oxyTile := d.Field[d.OxygenSystem]
	fmt.Printf("Part 1: %d\n", oxyTile.Dist)

	d.BFSFrom(d.OxygenSystem)
	fmt.Printf("Part 2: %d\n", d.MaxDist())
}
