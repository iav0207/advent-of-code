package main

import "slices"

type Result struct {Output []Word; Halted bool}

func run(code Code, input Word) (r Result) {
    code = slices.Clone(code)
    scanner := NewScanner(code)
    memory := Memory(code)
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
            *refs[0] = input
        case Output:
            r.Output = append(r.Output , *refs[0])
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
            r.Halted = true
			break eval
		default:
			fatalf("eval loop: unknown opcode %d\n", op)
		}
	}
    return
}
