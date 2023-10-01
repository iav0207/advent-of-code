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

const (
	phaseLimit = 5
	ampCount   = 5
)

var combCount = int(math.Pow(float64(phaseLimit), float64(ampCount)))

func main() {
	debugMode = isDebugMode()
	code := readInput()

	var bestOutput word
	var bestCombination int
	for c := 0; c < combCount; c++ {
		phases := phaseCombination(c)
		if countDistinct(phases) < ampCount {
			continue
		}
		debugf("c=%d\tp=%v\n", c, phases)
		amps := make([]*Amp, ampCount)
		for a := 0; a < ampCount; a++ {
			amps[a] = NewAmp(phases[a], NewExecution(code))
		}
		for a := 0; a < ampCount; a++ {
			if a < len(amps)-1 {
				amps[a].output = amps[a+1].input
			}
			amps[a].Launch()
			amps[a].input <- amps[a].p
		}

		amps[0].input <- word(0)
		output := <-amps[len(amps)-1].output

		if output > bestOutput {
			bestOutput = output
			bestCombination = c
		}
	}
	fmt.Printf("Part 1: highest signal is %d, sequence %v\n", bestOutput, phaseCombination(bestCombination))
}

type Amp struct {
	p      Phase
	e      *Execution
	input  chan word
	output chan word
}

func NewAmp(p Phase, e *Execution) *Amp {
	return &Amp{p, e, make(chan word), make(chan word)}
}

func (a *Amp) Launch() {
	go func() {
		defer close(a.output)
		for signal := range a.e.Run(a.input) {
			a.output <- signal
		}
	}()
}

type Phase = word

func phaseCombination(i int) []Phase {
	phases := make([]Phase, ampCount)
	for p := range phases {
		phases[p] = Phase((i / int(math.Pow(float64(phaseLimit), float64(p)))) % phaseLimit)
	}
	return phases
}

func countDistinct(phases []Phase) int {
	set := make(map[Phase]bool)
	for _, p := range phases {
		set[p] = true
	}
	return len(set)
}

func NewExecution(code []word) *Execution {
	codeCopy := make([]word, len(code))
	copy(codeCopy, code)
	return &Execution{p: Program{codeCopy}, scanner: &Scanner{sequence: codeCopy}}
}

func (e Execution) Run(input <-chan word) <-chan word {
	output := make(chan word)
	go func() {
		defer close(output)
	RUN:
		for {
			cmd := e.newCommand()
			params := cmd.params()
			args := e.args(cmd)

			switch cmd.op() {

			case Halt:
				debug("Halt.")
				break RUN

			case Add:
				debugf("added %d+%d=%d, put in %d (overwriting value %d)\n",
					*args[0], *args[1], *args[0]+*args[1], params[2], e.at(params[2]))
				*args[2] = *args[0] + *args[1]

			case Multiply:
				debugf("multiplied %d*%d=%d, put in %d (overwriting value %d)\n",
					*args[0], *args[1], *args[0]**args[1], params[2], e.at(params[2]))
				*args[2] = *args[0] * *args[1]

			case JumpIfTrue:
				if *args[0] != 0 {
					e.scanner.SetCursor(int(*args[1]))
				}

			case JumpIfFalse:
				if *args[0] == 0 {
					e.scanner.SetCursor(int(*args[1]))
				}

			case LessThan:
				*args[2] = 0
				if *args[0] < *args[1] {
					*args[2] = 1
				}

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

			default:
				log.Fatalf("Can't run opcode %d\n", cmd.op())
			}
		}
	}()
	return output
}

// newCommand scans the next command with the length depending on the count
// of the operation parameters.
func (e *Execution) newCommand() command {
	scanLen := Instruction{e.at(int64(e.scanner.cursor))}.Operation().ParCount + 1
	return command{code: e.scanner.Scan(scanLen)}
}

// args returns an array of pointers to the arguments. The pointers refer
// either to literal values, or to memory elements, depending on the modes
// in which parameters come in the command. This way, the execution
// further on is agnostic of the parameter modes, as it can always work
// on dereferenced argument values, for both reads and writes.
func (e *Execution) args(c command) []*word {
	memory := e.p.Code
	p, m := c.params(), c.modes()
	args := make([]*word, len(p))
	argsDebug := make([]word, len(p))
	for i := range p {
		if ParMode(m[i]) == Immediate {
			val := p[i]
			args[i] = &val // pointer to a value
		} else {
			args[i] = &memory[p[i]] // pointer to the memory element (writable)
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
}

func (e Execution) at(pos word) word {
	return e.p.Code[pos]
}

type Program struct {
	Code []word
}

type word = int64

type Execution struct {
	p       Program
	scanner *Scanner
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

func (s *Scanner) SetCursor(position int) {
	s.cursor = position
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
	Add         Operation = Operation{1, 3}
	Multiply    Operation = Operation{2, 3}
	Input       Operation = Operation{3, 1}
	Output      Operation = Operation{4, 1}
	JumpIfTrue  Operation = Operation{5, 2}
	JumpIfFalse Operation = Operation{6, 2}
	LessThan    Operation = Operation{7, 3}
	Equals      Operation = Operation{8, 3}
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
