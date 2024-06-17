package main

import (
	"fmt"
	"strings"
)

type Field map[Point]Word
type Point struct{ X, Y Word }
type Tile struct {
	Name string
	Repr string
}

func (p Point) Minus(o Point) Point { return p.Plus(o.Negate()) }
func (p Point) Plus(o Point) Point  { return Point{p.X + o.X, p.Y + o.Y} }
func (p Point) Negate() Point       { return Point{-p.X, -p.Y} }
func (p Point) String() string      { return fmt.Sprintf("(%d,%d)", p.X, p.Y) }

var (
	Up    = Point{0, 1}
	Right = Point{1, 0}
	Down  = Point{0, -1}
	Left  = Point{-1, 0}

	Clockwise = []Point{Up, Right, Down, Left}
)

const (
	TurnLeft  = False
	TurnRight = True
)

func (f Field) String() string {
	min, max := f.Limits()
	var sb strings.Builder
	for y := max.Y; y >= min.Y; y-- {
		for x := min.X; x <= max.X; x++ {
			code := f[Point{x, y}]
			tile := Tiles[int(code)]
			sb.WriteString(tile.Repr)
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (f Field) Limits() (min Point, max Point) {
	for panel := range f {
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
	return
}

func (f Field) CountTiles(name string) (c int) {
	for _, tileCode := range f {
		if Tiles[tileCode].Name == name {
			c++
		}
	}
	return
}

var Tiles = []Tile{
	{"empty", " "},
	{"wall", "■"},
	{"block", "★"},
	{"hpaddle", "—"},
	{"ball", "☻"},
}
