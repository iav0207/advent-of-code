package main

func run(code Code) {
	cursor := 0
eval:
	for {
		op := code[cursor]
		switch op {
		case Add:
			a, b, c := code[cursor+1], code[cursor+2], code[cursor+3]
			code[c] = code[a] + code[b]
			cursor += 4
		case Multiply:
			code[code[cursor+3]] = code[code[cursor+1]] * code[code[cursor+2]]
			cursor += 4
		case Halt:
			break eval
		default:
			fatalf("eval loop: unknown opcode %d\n", op)
		}
	}
}
