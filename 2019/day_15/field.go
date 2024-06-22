package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type (
	Point struct{ X, Y Word }
	Dist  int
	Direc struct {
		Code  Word
		Delta Point
	}
	Field map[Point]Tile

	Tile = Word
)

const (
	Wall         Word = 0
	Open         Word = 1
	OxygenSystem Word = 2
)

var (
	North = Direc{1, Point{0, 1}}
	South = Direc{2, Point{0, -1}}
	West  = Direc{3, Point{-1, 0}}
	East  = Direc{4, Point{1, 0}}

	Directions = []Direc{North, West, South, East}
)

func (d Direc) Reverse() Direc {
	search := d.Delta.Negate()
	for _, o := range Directions {
		if o.Delta == search {
			return o
		}
	}
	fatalf("cannot find reverse Direc for %v", d)
	return Direc{}
}

func (p Point) Minus(o Point) Point { return p.Plus(o.Negate()) }
func (p Point) Plus(o Point) Point  { return Point{p.X + o.X, p.Y + o.Y} }
func (p Point) Negate() Point       { return Point{-p.X, -p.Y} }
func (p Point) String() string      { return fmt.Sprintf("(%d,%d)", p.X, p.Y) }

func (t Tile) String() string {
	if t == Wall {
		return "#"
	}
	return "."
}

func (f Field) String() string {
	min, max := f.Limits()
	var sb strings.Builder
	sb.WriteString("     \t")
	for x := min.X; x <= max.X; x++ {
		sb.WriteString(strconv.FormatInt(int64(math.Abs(float64(x%10))), 10))
	}
	sb.WriteRune('\n')
	for y := max.Y; y >= min.Y; y-- {
		sb.WriteString(fmt.Sprintf("%*d:\t", 4, y))
		for x := min.X; x <= max.X; x++ {
			if tile, ok := f[Point{x, y}]; ok {
				sb.WriteString(tile.String())
			} else {
				sb.WriteRune(' ')
			}
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
