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

func NewExecution(code []int) *Execution {
	codeCopy := make([]int, len(code))
	copy(codeCopy, code)
	return &Execution{p: Program{codeCopy}}
}

func (e Execution) Run() {
RUN:
	for {
		inst := Instruction{e.scanOne()}
		op := inst.Operation()
		params, modes := e.scan(op.ParCount), inst.ParModes()

		arg := func(i int) *int {
			if ParMode(modes[i]) == Immediate {
				val := e.at(params[i])
				return &val
			}
			return &e.p.Code[params[i]]
		}

		switch op {
		case Halt:
			break RUN
		case Add:
			e.p.Code[*arg(2)] = *arg(0) + *arg(1)
		case Multiply:
			e.p.Code[*arg(2)] = *arg(0) * *arg(1)
		default:
			log.Fatalf("Can't run opcode %d\n", op)
		}
	}
}

func (e Execution) scanOne() int {
	return e.scan(1)[0]
}

func (e Execution) scan(count int) []int {
	buffer := make([]int, count)
	copy(buffer, e.p.Code[e.cursor:e.cursor+count])
	e.cursor += count
	return buffer
}

func (e Execution) at(pos int) int {
	return e.p.Code[pos]
}

type Program struct {
	Code []int
}

type Data struct {
	Content []int
}

type Execution struct {
	p      Program
	cursor int
}

type Instruction struct {
	Code int
}

func (i Instruction) Operation() Operation {
	return OperationFromCode(i.Code % 100)
}

func (i Instruction) ParModes() []int {
	digitAt := func(idx int) int { return i.Code / int(math.Pow10(idx)) % 10 }
	var modes []int
	for idx := 2; idx < 6; idx++ {
		modes = append(modes, digitAt(idx))
	}
	return modes
}

type Operation struct {
	Code     int
	ParCount int
}

type ParMode int

const (
	Position  ParMode = 0
	Immediate ParMode = 1
)

var (
	Add      Operation = Operation{1, 4}
	Multiply Operation = Operation{2, 4}
	Input    Operation = Operation{3, 1}
	Output   Operation = Operation{4, 1}
	Halt     Operation = Operation{99, 0}

	ops map[int]Operation = map[int]Operation{
		1:  Add,
		2:  Multiply,
		3:  Input,
		4:  Output,
		99: Halt,
	}
)

func OperationFromCode(code int) Operation {
	op, ok := ops[code]
	if !ok {
		panic(fmt.Sprintf("wrong opcode: %d", code))
	}
	return op
}

func readInput() []int {
	var line string
	_, err := fmt.Scanln(&line)
	failIf(err)
	numbers := strings.Split(line, ",")
	code := make([]int, 0)
	for _, num := range numbers {
		val, err := strconv.Atoi(num)
		failIf(err)
		code = append(code, val)
	}
	return code
}

func debug(a any) {
	if debugMode {
		fmt.Printf("%v\n", a)
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
