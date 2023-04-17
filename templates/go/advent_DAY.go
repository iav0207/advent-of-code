package main

import (
	"fmt"
	"os"
)

var debugMode bool

func main() {
	debugMode = isDebugMode()
}

func readInputLines() []string {
	lines := make([]string, 0)
	var line string
	for {
		_, err := fmt.Scanf("%s\n", &line)
		if err != nil {
			break
		}
		lines = append(lines, line)
	}
	return lines
}

func isDebugMode() bool {
	for _, arg := range os.Args[1:] {
		if arg == "-d" {
			return true
		}
	}
	return false
}
