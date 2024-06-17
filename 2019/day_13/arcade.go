package main

import (
	"fmt"
	"time"
)

type Arcade struct {
	Eval  Eval
	Field Field
	Score Word
}

func NewArcade(c Code) *Arcade {
	return &Arcade{Eval: *NewEval(c), Field: make(Field)}
}

func (a *Arcade) Run() {
	e, f := a.Eval, a.Field
	e.Start()
	var paddlePos, ballPos Point
	var step int
	for !e.Halted {
		p, t := Point{X: <-e.Out, Y: -(<-e.Out)}, <-e.Out
		if p == ScoreCode {
			a.Score = t
		} else {
			f[p] = t
			if Tiles[t].Name == "hpaddle" {
				paddlePos = p
				if step > 0 {
					a.Redraw()
					time.Sleep(15 * time.Millisecond)
				}
				step++
				debugf("arcade: step %d\n", step)
			} else if Tiles[t].Name == "ball" {
				ballPos = p
				joystick := sign(ballPos.X - paddlePos.X)
				if ballPos.Y-paddlePos.X == 1 {
					joystick *= -1
				}
				e.In <- joystick
				debugf("arcade: bX=%d pX=%d j=%d\n", ballPos.X, paddlePos.X, joystick)
			}
		}
	}
}

func (a *Arcade) Redraw() {
	fmt.Print("\033[H\033[2J")
	fmt.Println(a.Field)
	fmt.Printf("Score: %d\n", a.Score)
}

func sign(w Word) Word {
	if w < 0 {
		return -1
	} else if w > 0 {
		return 1
	}
	return w
}

var ScoreCode = Point{-1, 0}
