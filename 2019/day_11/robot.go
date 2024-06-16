package main

import (
	"fmt"
	"slices"
)

type Robot struct {
	Position  Point
	Direction Point

	Eval  *Eval
	Field Field
}

func NewRobot(program Code, field Field) *Robot {
	return &Robot{
		Direction: Up,
		Eval:      NewEval(program),
		Field:     field,
	}
}

func (r *Robot) Run() {
    e := r.Eval
	e.Start()
	for {
        debugf("robot: at %s facing %s\n", r.Position, r.Direction)
	    e.In <- r.Field[r.Position]
		color, ok := <-e.Out
		if !ok {
			break
		}
        if color == White {
            debugln("robot: paint white")
        } else {
            debugln("robot: paint black")
        }
        r.Field[r.Position] = color
        turn := <-e.Out
        r.Turn(turn)
        r.Advance()
	}
    debugln("robot: done painting")
}

func (r *Robot) Turn(t Word) {
	d := slices.Index(Clockwise, r.Direction)
	inc := 1
	if t == TurnLeft {
		inc = -1
        debugln("robot: turn left")
	} else {
        debugln("robot: turn right")
    }
    d += inc+len(Clockwise)
    d %= len(Clockwise)
	r.Direction = Clockwise[d]
}

func (r *Robot) Advance() {
	r.Position = r.Position.Plus(r.Direction)
}

type Field map[Point]Word
type Point struct{ X, Y Word }

func (f Field) PaintedPanelCount() int { return len(f) }

func (p Point) Plus(o Point) Point { return Point{p.X + o.X, p.Y + o.Y} }
func (p Point) String() string { return fmt.Sprintf("(%d,%d)", p.X, p.Y) }

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
