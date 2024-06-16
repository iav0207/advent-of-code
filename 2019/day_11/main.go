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

	field := make(Field)
	NewRobot(code, field).Run()
	fmt.Printf("Part 1: %d\n", field.PaintedPanelCount())

	field = make(Field)
	robot := NewRobot(code, field)
	field[robot.Position] = White
	robot.Run()

	min, max := Point{}, Point{}
	for panel := range field {
		if panel.X < min.X {
			min.X = panel.X
		}
		if panel.X > max.X {
			max.X = panel.X
		}
		if panel.Y < min.Y {
			min.Y = panel.Y
		}
		if panel.Y > max.Y {
			max.Y = panel.Y
		}
	}
	fmt.Println("Part 2")
	for y := max.Y; y >= min.Y; y-- {
		for x := min.X; x <= max.X; x++ {
			color := field[Point{x, y}]
            tile := " "
			if color == White {
				tile = "■"
			}
			fmt.Print(tile)
		}
		fmt.Println()
	}
}

const (
	Black = False
	White = True
)
