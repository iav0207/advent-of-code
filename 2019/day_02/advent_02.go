package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

var debugMode bool

func main() {
	debugMode = isDebugMode()
	code := readInput()

	result := executeWith(code, 12, 2)
	fmt.Printf("Part 1: %d\n", result[0])

	var noun, verb int
nounloop:
	for noun = 0; noun < 100; noun++ {
		for verb = 0; verb < 100; verb++ {
			result := executeWith(code, noun, verb)
			if result[0] == 19690720 {
				break nounloop
			}
		}
	}
	fmt.Printf("Part 2: %d\n", noun*100+verb)
}

func executeWith(code []int, noun int, verb int) []int {
	buffer := make([]int, len(code))
	copy(buffer, code)
	buffer[1] = noun
	buffer[2] = verb

	for i := 0; buffer[i] != 99; i += 4 {
		params := buffer[i+1 : i+4]
		switch val := buffer[i]; val {
		case 1:
			buffer[params[2]] = buffer[params[0]] + buffer[params[1]]
		case 2:
			buffer[params[2]] = buffer[params[0]] * buffer[params[1]]
		default:
			log.Fatalf("Unknown opcode %d\n", val)
		}
	}

	return buffer
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
