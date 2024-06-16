package main

import (
	"fmt"
	"math"
)

type Inst Code
type Op struct {
	Code       Word
	Name       string
	ParamCount int
}
type ParamMode Word
type Param struct {
	Val  Word
	Mode ParamMode
}

func (i Inst) Op() Op {
	opCode := i.InstCode() % 100
	op, ok := opFromCode[opCode]
	if !ok {
		fatalf("inst %d: opFromCode[%d] is not found\n", i, opCode)
	}
	return op
}

func (i Inst) InstCode() Word { return i[0] }

func (i Inst) Params() []Param {
	var params []Param
	instCode := i.InstCode()
	for p, val := range i[1:] {
		params = append(params, Param{
			Val:  val,
			Mode: ParamMode(instCode.digitAt(2 + p)),
		})
	}
	return params
}

func (w Word) digitAt(i int) Word {
	return (w / Word(int64(math.Pow10(i)))) % 10
}

var (
	opFromCode = map[Word]Op{
		1:  Add,
		2:  Multiply,
		3:  Input,
		4:  Output,
		5:  JumpIf,
		6:  JumpUnless,
		7:  LessThan,
		8:  Equals,
		9:  Rebase,
		99: Halt,
	}

	Add        Op = Op{1, "Add", 3}
	Multiply   Op = Op{2, "Multiply", 3}
	Input      Op = Op{3, "Input", 1}
	Output     Op = Op{4, "Output", 1}
	JumpIf     Op = Op{5, "JumpIfTrue", 2}
	JumpUnless Op = Op{6, "JumpIfFalse", 2}
	LessThan   Op = Op{7, "LessThan", 3}
	Equals     Op = Op{8, "Equals", 3}
	Rebase     Op = Op{9, "Rebase", 1}
	Halt       Op = Op{99, "Halt", 0}

	Position  ParamMode = 0
	Immediate ParamMode = 1
	Relative  ParamMode = 2
)

func (op Op) String() string {
	return fmt.Sprintf("%s(%d)", op.Name, op.Code)
}

func (i Inst) String() string {
	return fmt.Sprintf("Inst: src=%v, op=%s, params=%v", Code(i), i.Op(), i.Params())
}

func (p Param) String() string {
	var prefix string
	if p.Mode == Position {
		prefix = "@"
	}
	return fmt.Sprintf("Param{%s%d}", prefix, p.Val)
}
