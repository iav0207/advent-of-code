package main

type OpCode int

const (
	Add      = 1
	Multiply = 2
	Halt     = 99
)

func paramsCount(op OpCode) (c int) {
    c, ok := paramCount[op]
    if !ok {
		fatalf("ParamsCount: unknown opcode %d\n", op)
	}
	return
}

var paramCount = map[OpCode]int{
    Add: 3,
    Multiply: 3,
    Halt: 0,
}
