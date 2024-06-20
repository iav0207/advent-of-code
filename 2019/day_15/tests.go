package main

import (
	"os"
	"reflect"
)

var doTest = false

func init() {
	for _, arg := range os.Args[1:] {
		if arg == "-t" || arg == "--test" {
			doTest = true
			break
		}
	}
	if doTest {
		test()
		os.Exit(0)
	}
}

func test() {
	cases := []struct {
		code       string
		input      []Word
		wantOutput []Word
		wantCode   Code
	}{
		// Using position mode, consider whether the input is equal to 8;
		// output 1 (if it is) or 0 (if it is not).
		{
			code:       "3,9,8,9,10,9,4,9,99,-1,8",
			input:      Words(7),
			wantOutput: Words(0),
		},
		{
			code:       "3,9,8,9,10,9,4,9,99,-1,8",
			input:      Words(8),
			wantOutput: Words(1),
		},
		{
			code:       "3,9,8,9,10,9,4,9,99,-1,8",
			input:      Words(9),
			wantOutput: Words(0),
		},
		// Using position mode, consider whether the input is less than 8;
		// output 1 (if it is) or 0 (if it is not).
		{
			code:       "3,9,7,9,10,9,4,9,99,-1,8",
			input:      Words(7),
			wantOutput: Words(1),
		},
		{
			code:       "3,9,7,9,10,9,4,9,99,-1,8",
			input:      Words(8),
			wantOutput: Words(0),
		},
		{
			code:       "3,9,7,9,10,9,4,9,99,-1,8",
			input:      Words(9),
			wantOutput: Words(0),
		},
		// Using immediate mode, consider whether the input is equal to 8;
		// output 1 (if it is) or 0 (if it is not).
		{
			code:       "3,3,1108,-1,8,3,4,3,99",
			input:      Words(7),
			wantOutput: Words(0),
		},
		{
			code:       "3,3,1108,-1,8,3,4,3,99",
			input:      Words(8),
			wantOutput: Words(1),
		},
		{
			code:       "3,3,1108,-1,8,3,4,3,99",
			input:      Words(9),
			wantOutput: Words(0),
		},
		// The program will then output 999 if the input value is below 8,
		// output 1000 if the input value is equal to 8,
		// or output 1001 if the input value is greater than 8.
		{
			code:       "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
			input:      Words(-1),
			wantOutput: Words(999),
		},
		{
			code:       "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
			input:      Words(8),
			wantOutput: Words(1000),
		},
		{
			code:       "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
			input:      Words(9),
			wantOutput: Words(1001),
		},
		// Takes no input and produces a copy of itself as output.
		{
			code:       "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
			wantOutput: Words(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99),
		},
        {
            // should output the large number in the middle
            code: "104,1125899906842624,99",
            wantOutput: Words(1125899906842624),
        },
	}

	for i, tc := range cases {
		e := NewEval(asCode(tc.code))
		for _, val := range tc.input {
			e.In <- val
		}
		e.Start()
        gotOutput := e.CollectOutput()
		if !reflect.DeepEqual(gotOutput, tc.wantOutput) {
			fatalf("test case #%d:\ngot  = %v\nwant = %v\n", i, gotOutput, tc.wantOutput)
		}
	}
}
