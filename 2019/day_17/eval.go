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
	memory := NewMemory(e.Code)
	defer close(e.Out)
	defer close(e.Done)
eval:
	for {
		inst := scanner.Next()
		debugln(inst)
		op := inst.Op()
		params := inst.Params()
		vals := memory.GetAll(params...)
		switch op {
		case Add:
			memory.Put(params[2], vals[0]+vals[1])
		case Multiply:
			memory.Put(params[2], vals[0]*vals[1])
		case Input:
			memory.Put(params[0], <-e.In)
		case Output:
			e.Out <- vals[0]
		case JumpIf:
			if vals[0] != 0 {
				scanner.Cursor = int(vals[1])
			}
		case JumpUnless:
			if vals[0] == 0 {
				scanner.Cursor = int(vals[1])
			}
		case LessThan:
			memory.Put(params[2], If(vals[0] < vals[1]))
		case Equals:
			memory.Put(params[2], If(vals[0] == vals[1]))
		case Rebase:
			memory.Base += vals[0]
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

func (e *Eval) CollectOutput() (out []Word) {
	for {
		val, ok := <-e.Out
		if !ok {
			break
		}
		out = append(out, val)
	}
    return
}
