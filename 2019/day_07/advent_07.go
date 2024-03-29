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

	for part := 1; part < 3; part++ {
		s := Solution{part, code}
		bestOutput, bestConfig := s.FindBestConfiguration()
		fmt.Printf("Part %d: highest signal is %d, sequence %v\n", part, bestOutput, bestConfig)
	}
}

type Solution struct {
	part int
	code []word
}

// FindBestConfiguration assembles the circuit for every possible phase config
// and evaluates its output. Returns the maximum output and the corresponding
// circuit configuration.
func (s Solution) FindBestConfiguration() (bestOutput word, bestConfig CircuitConfig) {
	for permutation := range permute(ampCount) {
		phases := make([]Phase, ampCount)
		amps := make([]*Amp, ampCount)
		for a := 0; a < ampCount; a++ {
			phases[a] = Phase(permutation[a])
			if s.part > 1 {
				phases[a] += Phase(5)
			}
			amps[a] = NewAmp(Phase(phases[a]), NewExecution(s.code))
		}

		output := s.evalCircuit(amps)

		if output > bestOutput {
			bestOutput = output
			bestConfig = phases
		}
	}
	return
}

type CircuitConfig []Phase

func permute(n int) <-chan []int {
	pool := make([]int, n)
	for i := 0; i < n; i++ {
		pool[i] = i
	}
	ch := make(chan []int)
	go func() {
		defer close(ch)
		for permutation := range permuteRec(pool) {
			ch <- permutation
		}
	}()
	return ch
}

func permuteRec(pool []int) <-chan []int {
	ch := make(chan []int)
	go func() {
		defer close(ch)
		for pi, p := range pool {
			next := append([]int{}, pool[:pi]...)
			next = append(next, pool[pi+1:]...)
			for sub := range permuteRec(next) {
				ch <- append([]int{p}, sub...)
			}
		}
		if len(pool) == 0 {
			ch <- nil
		}
	}()
	return ch
}

// Runs signal through the circuit laid out according to the specification
// of the puzzle part: linear sequence or a feedback loop.
// Returns the output signal from the last amp in the slice.
func (s Solution) evalCircuit(amps []*Amp) (result word) {
	signal := word(0)
	step := 0
	for {
		if s.part == 1 && step >= len(amps) {
			return
		}
		a := step % len(amps)
		amp := amps[a]
		go func() { // Async send here prevents deadlocks.
			amp.input <- signal
		}()
		select {
		case signal = <-amp.output:
			if a == len(amps)-1 { // The last amp output is the result.
				result = signal
			}
		case <-amp.done:
			debugf("amp %d terminated.\n", amp.p)
			return
		}
		step++
	}
}

// Amp is an intcode computer running one Execution of a Program.
// It has one configuration set initially - its phase. Phase always
// becomes the first input value to the program, given exactly once.
// After that Amp receives inputs through the input channel. As the
// Execution runs, output values get pushed to the output channel
// until the Program terminates, in which case a true value gets
// pushed onto the done channel of the Amp.
type Amp struct {
	p      Phase
	e      *Execution
	input  chan word
	output <-chan word
	done   chan bool
}

// NewAmp instantiates a running Amp with its phase passed down
// its input channel.
func NewAmp(p Phase, e *Execution) *Amp {
	input := make(chan word)
	output, done := e.Run(input)
	input <- p
	return &Amp{p, e, input, output, done}
}

func (a Amp) Input() chan<- word  { return a.input }
func (a Amp) Output() <-chan word { return a.output }
func (a Amp) Done() <-chan bool   { return a.done }

type Phase = word

// NewExecution instantiates an execution of a copy of the given code.
func NewExecution(code []word) *Execution {
	codeCopy := make([]word, len(code))
	copy(codeCopy, code)
	return &Execution{p: Program{codeCopy}, scanner: &Scanner{sequence: codeCopy}}
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
	return output, done
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

func (c command) params() []word           { return c.code[1:] }
func (c command) modes() []word            { return c.instruction().ParModes() }
func (c command) op() Operation            { return c.instruction().Operation() }
func (c command) instruction() Instruction { return Instruction{c.code[0]} }

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
