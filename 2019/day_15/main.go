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

    var oxygenSystem Point
	var oxyDist Dist
	for point, dist := range d.BFSFrom(d.Position) {
		if tile := d.Field[point]; tile == OxygenSystem {
            oxygenSystem = point
			oxyDist = dist
			break
		}
	}
	fmt.Printf("Part 1: %d\n", oxyDist)

	var maxDist Dist
	for _, dist := range d.BFSFrom(oxygenSystem) {
		if dist > maxDist {
			maxDist = dist
		}
	}
	fmt.Printf("Part 2: %d\n", maxDist)
}
