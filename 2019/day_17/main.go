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

	e := NewEval(code)
	e.Start()
	f := make(Field)
	var row, col Word
	var robotPos Point
	var robotDir Direc

	for output := range e.Out {
		r := string(rune(byte(output)))
		fmt.Print(r)
		switch r {
		case "\n":
			row++
			col = 0
            continue
		case Scaffold:
			f[Point{col, row}] = r
		case Open:
		default:
			robotPos = Point{col, row}
			robotDir = DirecFrom(r)
		}
		col++
	}
	debugln(robotPos, robotDir)

	var alignment int
	for p := range f {
		if f.IsIntersection(p) {
			alignment += int(p.X) * int(p.Y)
			fmt.Printf("intersection at %s, alignment + %d = %d\n", p, p.X*p.Y, alignment)
		}
	}

	fmt.Printf("Part 1: %d\n", alignment)

}
