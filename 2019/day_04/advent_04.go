package main

import (
	"fmt"
	"log"
	"math"
	"os"
)

var debugMode bool

func main() {
	debugMode = isDebugMode()
	var from, to int
	_, err := fmt.Scanf("%d-%d\n", &from, &to)
	failIf(err)

	digits := asArray(from)

	// digits should not be decreasing
	for pos := 0; pos < len(digits); pos++ {
		if pos > 0 && digits[pos] < digits[pos-1] {
			digits[pos] = digits[pos-1]
		}
	}

	// at least two adjacent digits are the same
	hasDoublePart1 := func() bool {
		for i := 1; i < len(digits); i++ {
			if digits[i] == digits[i-1] {
				return true
			}
		}
		return false
	}

	// there is an occurrence of exactly two equal adjacent digits
	hasDoublePart2 := func() bool {
		streak := 1
		for i := 1; i < len(digits); i++ {
			if digits[i] == digits[i-1] {
				streak++
			} else {
				if streak == 2 {
					return true
				}
				streak = 1
			}
		}
		return streak == 2
	}

	matchCount := make([]int, 2) // one per puzzle part
	for {
		// check range
		if asInt(digits) > to {
			break
		}
		// count if matches
		if hasDoublePart1() {
			matchCount[0]++
			if hasDoublePart2() {
				matchCount[1]++
			}
		}
		// find lowest uppable digit
		var d int
		for p := len(digits) - 1; p >= 0; p-- {
			if digits[p] < 9 {
				d = p
				break
			}
		}
		// up it
		digits[d]++
		// reset the lower digits to the new value of digit d
		for p := d + 1; p < len(digits); p++ {
			digits[p] = digits[d]
		}
	}
	fmt.Printf("Part 1: %d\n", matchCount[0])
	fmt.Printf("Part 2: %d\n", matchCount[1])
}

func asArray(n int) []int {
	digits := make([]int, len(fmt.Sprint(n)))
	for i := 0; i < len(digits); i++ {
		digits[i] = (n / int(math.Pow10(len(digits)-1-i))) % 10
	}
	return digits
}

func asInt(array []int) int {
	var n int
	for i := 0; i < len(array); i++ {
		n += array[i] * int(math.Pow10(len(array)-1-i))
	}
	return n
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
