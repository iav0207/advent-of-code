package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

var debugMode bool

func main() {
	debugMode = isDebugMode()
	code := readInput()

	NewExecution(code).Run()
}

func NewExecution(code []word) *Execution {
	codeCopy := make([]word, len(code))
	copy(codeCopy, code)
	return &Execution{p: Program{codeCopy}}
}

func (e Execution) Run() {
RUN:
	for {
		cmd := e.newCommand()
		params := cmd.params()
		args := cmd.args()

		switch cmd.op() {
		case Halt:
			fmt.Println("Halt.")
			break RUN
		case Add:
			debugf("added %d+%d=%d, put in %d (overwriting value %d)\n",
				*args[0], *args[1], *args[0]+*args[1], params[2], e.at(params[2]))
			*args[2] = *args[0] + *args[1]
		case Multiply:
			debugf("multiplied %d*%d=%d, put in %d (overwriting value %d)\n",
				*args[0], *args[1], *args[0]**args[1], params[2], e.at(params[2]))
			*args[2] = *args[0] * *args[1]
		case Input:
			*args[0] = 1
			debugf("put 1 in %d\n", params[0])
		case Output:
			fmt.Printf("Output: %d\n\n", *args[0])
		default:
			log.Fatalf("Can't run opcode %d\n", cmd.op())
		}
	}
}

func (e *Execution) newCommand() command {
	scanLen := Instruction{e.at(int64(e.cursor))}.Operation().ParCount + 1
	cmd := e.scan(scanLen)
	return command{
		code: cmd,
		mem:  e.p.Code,
	}
}

func (c command) args() []*word {
	p, m := c.params(), c.modes()
	args := make([]*word, len(p))
	argsDebug := make([]word, len(p))
	for i := range p {
		if ParMode(m[i]) == Immediate {
			val := p[i]
			args[i] = &val // pointer to a value
		} else {
			args[i] = &c.mem[p[i]] // pointer to the memory element (writable)
		}
		argsDebug[i] = *args[i]
	}

	debugf("inst=%v\top=%v\tparams=%v\tmodes=%v\targs=%v\n",
		c.instruction(), c.op(), p, m, argsDebug)
	return args
}

func (c command) params() []word {
	return c.code[1:]
}

func (c command) modes() []word {
	return c.instruction().ParModes()
}

func (c command) op() Operation {
	return c.instruction().Operation()
}

func (c command) instruction() Instruction {
	return Instruction{c.code[0]}
}

type command struct {
	code []word
	mem  []word
}

func (e *Execution) scanOne() word {
	return e.scan(1)[0]
}

func (e *Execution) scan(count int) []word {
	buffer := make([]word, count)
	copy(buffer, e.p.Code[e.cursor:e.cursor+count])
	debugf("scanned %d from %d: %v\n", count, e.cursor, buffer)
	e.cursor += count
	return buffer
}

func (e Execution) at(pos word) word {
	return e.p.Code[pos]
}

type Program struct {
	Code []word
}

type word = int64

type Data struct {
	Content []int
}

type Execution struct {
	p      Program
	cursor int
}

type Instruction struct {
	Code word
}

func (i Instruction) Operation() Operation {
	return OperationFromCode(i.Code % 100)
}

func (i Instruction) ParModes() []word {
	digitAt := func(idx int) word { return i.Code / word(math.Pow10(idx)) % 10 }
	var modes []word
	for idx := 2; idx < 6; idx++ {
		modes = append(modes, digitAt(idx))
	}
	return modes
}

type Operation struct {
	Code     word
	ParCount int
}

type ParMode int

const (
	Position  ParMode = 0
	Immediate ParMode = 1
)

var (
	Add      Operation = Operation{1, 3}
	Multiply Operation = Operation{2, 3}
	Input    Operation = Operation{3, 1}
	Output   Operation = Operation{4, 1}
	Halt     Operation = Operation{99, 0}

	ops map[word]Operation = map[word]Operation{
		1:  Add,
		2:  Multiply,
		3:  Input,
		4:  Output,
		99: Halt,
	}
)

func OperationFromCode(code word) Operation {
	op, ok := ops[code]
	if !ok {
		panic(fmt.Sprintf("wrong opcode: %d", code))
	}
	return op
}

func readInput() []word {
	var line string
	_, err := fmt.Scanln(&line)
	failIf(err)
	numbers := strings.Split(line, ",")
	code := make([]word, 0)
	for _, num := range numbers {
		val, err := strconv.Atoi(num)
		failIf(err)
		code = append(code, word(val))
	}
	return code
}

func debug(a any) {
	if debugMode {
		fmt.Printf("%v\n", a)
	}
}

func debugf(format string, a ...any) {
	if debugMode {
		fmt.Printf(format, a...)
	}
}

func isDebugMode() bool {
	for _, arg := range os.Args[1:] {
		if arg == "-d" {
			return true
		}
	}
	return false
}

func failIf(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
