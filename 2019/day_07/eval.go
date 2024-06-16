package main

import "slices"

type Eval struct {
	Code   Code
	In     chan Word
	Out    chan Word
	Done   chan bool
	Halted bool
}

func NewEval(c Code) *Eval {
	return &Eval{
		Code: slices.Clone(c),
		In:   make(chan Word, 2),
		Out:  make(chan Word, 1),
		Done: make(chan bool, 1),
	}
}

func (e *Eval) Start() { go e.Run() }

func (e *Eval) Run() {
	scanner := NewScanner(e.Code)
	memory := Memory(e.Code)
	defer close(e.Out)
	defer close(e.Done)
eval:
	for {
		inst := scanner.NextInst()
		debugln(inst)
		op := inst.Op()
		params := inst.Params()
		refs := memory.GetAll(params...)
		switch op {
		case Add:
			*refs[2] = *refs[0] + *refs[1]
		case Multiply:
			*refs[2] = *refs[0] * *refs[1]
		case Input:
			*refs[0] = <-e.In
		case Output:
			e.Out <- *refs[0]
		case JumpIf:
			if *refs[0] != 0 {
				scanner.Cursor = int(*refs[1])
			}
		case JumpUnless:
			if *refs[0] == 0 {
				scanner.Cursor = int(*refs[1])
			}
		case LessThan:
			*refs[2] = If(*refs[0] < *refs[1])
		case Equals:
			*refs[2] = If(*refs[0] == *refs[1])
		case Halt:
			e.Halted = true
			e.Done <- true
			break eval
		default:
			fatalf("eval loop: unknown opcode %d\n", op)
		}
	}
	return
}
