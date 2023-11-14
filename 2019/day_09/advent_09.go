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
	debugf("code length = %d\n", len(code))

	fmt.Printf("Part 1: %d\n", run(NewExecution(code), 1, runOpts{stopAtNonZero: false}))
}

type runOpts struct{ stopAtNonZero bool }

func run(e *Execution, input word, opts runOpts) []word {
	in := make(chan word)
	out, done := e.Run(in)
	var output []word
	go func() { // Async send here prevents deadlocks.
		in <- 1
	}()
	terminated := false
	for !terminated {
		select {
		case value := <-out:
			output = append(output, value)
			if opts.stopAtNonZero && value != 0 {
				return output
			}
		case <-done:
			terminated = true
		}
	}
	return output
}

// NewExecution instantiates an execution of a copy of the given code.
func NewExecution(code []word) *Execution {
	codeCopy := make([]word, len(code))
	copy(codeCopy, code)
	return &Execution{
		p:       Program{codeCopy, make(map[word]*word)},
		scanner: &Scanner{sequence: codeCopy},
	}
}

// Run is the main intcode program execution loop running in a goroutine.
// The input channel (argument) will be read from whenever the execution
// encounters input instructions. The output channel (first return value)
// will stream output of the execution, provided there are output instructions
// in the program. The termination channel (second return value) will
// stream exactly one true value if and when the program halts, otherwise
// it will stream no values.
func (e Execution) Run(input <-chan word) (<-chan word, chan bool) {
	output := make(chan word)
	done := make(chan bool)
	go func() {
		defer close(output)
		defer close(done)
		for {
			cmd := e.newCommand()
			params := cmd.params()
			args := e.args(cmd)

			switch cmd.op() {

			case Halt:
				debug("Halt.")
				done <- true
				return

			case Add:
				res := *args[0] + *args[1]
				*args[2] = res

			case Multiply:
				res := *args[0] * *args[1]
				// debugf("multiplied %d*%d=%d, put in %d (overwriting value %d)\n",
				// 	*args[0], *args[1], res, params[2], *e.p.At(params[2]))
				*args[2] = res

			case JumpIfTrue:
				if *args[0] != 0 {
					e.scanner.SetCursor(int(*args[1]))
				}

			case JumpIfFalse:
				if *args[0] == 0 {
					e.scanner.SetCursor(int(*args[1]))
				}

			case LessThan:
				res := word(0)
				if *args[0] < *args[1] {
					res = 1
				}
				*args[2] = res

			case Equals:
				*args[2] = 0
				if *args[0] == *args[1] {
					*args[2] = 1
				}

			case Input:
				*args[0] = <-input
				debugf("put %d in %d\n", *args[0], params[0])

			case Output:
				if *args[0] != 0 {
					debugf("Output: %d\n", *args[0])
				}
				output <- *args[0]

			case BaseShift:
				e.offset += *args[0]

			default:
				log.Fatalf("Can't run opcode %d\n", cmd.op())
			}
		}
	}()
	return output, done
}

// newCommand scans the next command with the length depending on the count
// of the operation parameters.
func (e *Execution) newCommand() command {
	code := *e.p.At(int64(e.scanner.cursor))
	scanLen := Instruction{code}.Operation().ParCount + 1
	return command{code: e.scanner.Scan(scanLen)}
}

// args returns an array of pointers to the arguments. The pointers refer
// either to literal values, or to memory elements, depending on the modes
// in which parameters come in the command. This way, the execution
// further on is agnostic of the parameter modes, as it can always work
// on dereferenced argument values, for both reads and writes.
func (e *Execution) args(c command) []*word {
	p, m := c.params(), c.modes()
	args := make([]*word, len(p))
	argsDebug := make([]word, len(p))
	for i := range p {
		mode := ParMode(m[i])
		switch mode {
		case Immediate:
			val := p[i]
			args[i] = &val // pointer to a value
		case Position:
			pos := p[i]
			args[i] = e.p.At(pos) // pointer to the memory element (writable)
		case Relative:
			relPos := p[i]
			absPos := e.offset + relPos
			args[i] = e.p.At(absPos)
		}
		argsDebug[i] = *args[i]
	}

	debugf("inst=%v\top=%v\tparams=%v\tmodes=%v\targs=%v\n",
		c.instruction(), c.op(), p, m, argsDebug)
	return args
	// return Args{param: p, mode: m, offset: e.offset, ref: args}
}

type Args struct {
	param  []word
	mode   []word
	offset word
	ref    []*word
}

func (a *Args) arg(i int) *word {
	return nil
}

func (c command) params() []word           { return c.code[1:] }
func (c command) modes() []word            { return c.instruction().ParModes() }
func (c command) op() Operation            { return c.instruction().Operation() }
func (c command) instruction() Instruction { return Instruction{c.code[0]} }

type command struct {
	code []word
}

type Program struct {
	code []word
	ram  map[word]*word
}

// At returns a pointer to the memory element. The element can be read from
// or written to, through the pointer.
func (p Program) At(pos word) *word {
	if pos > word(len(p.code)) {
		if v, ok := p.ram[pos]; ok {
			return v
		}
		v := new(word)
		p.ram[pos] = v
		return v
	}
	return &p.code[pos]
}

type word = int64

type Execution struct {
	p       Program
	scanner *Scanner
	offset  word
}

type Scanner struct {
	sequence []word
	cursor   int
}

func (s *Scanner) Scan(count int) []word {
	buffer := make([]word, count)
	copy(buffer, s.sequence[s.cursor:s.cursor+count])
	debugf("scanned %d from %d: %v\n", count, s.cursor, buffer)
	s.cursor += count
	return buffer
}

func (s *Scanner) SetCursor(position int) { s.cursor = position }

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
	Relative  ParMode = 2
)

var (
	Add         Operation = Operation{1, 3}
	Multiply    Operation = Operation{2, 3}
	Input       Operation = Operation{3, 1}
	Output      Operation = Operation{4, 1}
	JumpIfTrue  Operation = Operation{5, 2}
	JumpIfFalse Operation = Operation{6, 2}
	LessThan    Operation = Operation{7, 3}
	Equals      Operation = Operation{8, 3}
	BaseShift   Operation = Operation{9, 1}
	Halt        Operation = Operation{99, 0}

	ops map[word]Operation = map[word]Operation{
		1:  Add,
		2:  Multiply,
		3:  Input,
		4:  Output,
		5:  JumpIfTrue,
		6:  JumpIfFalse,
		7:  LessThan,
		8:  Equals,
		9:  BaseShift,
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
