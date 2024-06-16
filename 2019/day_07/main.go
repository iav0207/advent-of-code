package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	input, err := reader.ReadString('\n')
	fatalIf(err, "read stdin")
	code := asCode(strings.ReplaceAll(input, "\n", ""))

	maxOutput := Word(0)
	for phases := range permute(5, 1, Words(0, 1, 2, 3, 4)) {
		signal := Word(0)
		debugf("amps: %v\n", phases)
		for i, ph := range phases {
			debugln("new eval")
			e := NewEval(code)
			e.In <- ph
			e.In <- signal
			e.Run()
			for val := range e.Out {
				debugf("amp[%d]: rcv signal %d\n", i, signal)
				signal = val
			}
		}
		if signal > maxOutput {
			maxOutput = signal
		}
	}
	fmt.Printf("Part 1: %d\n", maxOutput)

	maxOutput = Word(0)
	for phases := range permute(5, 1, Words(5, 6, 7, 8, 9)) {
		n := len(phases)
		var amps []*Eval
		for _, ph := range phases {
			amp := NewEval(code)
			amp.In <- ph
			amps = append(amps, amp)
		}
		amps[0].In <- Word(0)
		for i := 0; i < n; i++ {
			amps[i].Out = amps[(i+1)%n].In
			amps[i].Start()
		}
		for _, amp := range amps {
			<-amp.Done
		}
	rcv:
		for {
			select {
			case output, ok := <-amps[n-1].Out:
				if !ok {
					break rcv
				}
				debugf("amp[4]: rcv %d\n", output)
				if output > maxOutput {
					maxOutput = output
				}
			}
		}
	}
	fmt.Printf("Part 2: %d\n", maxOutput)
}
