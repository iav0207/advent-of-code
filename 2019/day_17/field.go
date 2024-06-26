package main

import (
	"fmt"
	"math"
	"slices"
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

	Tile = string
)

const (
	Scaffold = "#"
	Open     = "."
)

var (
	Up    = Direc{1, Point{0, -1}}
	Down  = Direc{2, Point{0, 1}}
	Left  = Direc{3, Point{-1, 0}}
	Right = Direc{4, Point{1, 0}}

	Directions = []Direc{Up, Left, Down, Right}
	RobotChars = []string{"^", "<", "v", ">"}
)

func DirecFrom(s string) Direc { return Directions[slices.Index(RobotChars, s)] }

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
			if _, ok := f[Point{x, y}]; ok {
				sb.WriteString(Scaffold)
			} else {
				sb.WriteString(Open)
			}
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (f Field) IsIntersection(p Point) bool {
	if !f.Contains(p) {
		return false
	}
	for _, d := range Directions {
		adj := p.Plus(d.Delta)
		if !f.Contains(adj) {
			return false
		}
	}
	return true
}
func (f Field) Contains(p Point) bool {
	_, ok := f[p]
	return ok
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
