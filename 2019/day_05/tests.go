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
	}
}

func test() {
	cases := []struct {
		code       string
		input      Word
		wantOutput []Word
		wantCode   Code
	}{
		// Using position mode, consider whether the input is equal to 8;
		// output 1 (if it is) or 0 (if it is not).
		{
			code:       "3,9,8,9,10,9,4,9,99,-1,8",
			input:      Word(7),
			wantOutput: Words(0),
		},
		{
			code:       "3,9,8,9,10,9,4,9,99,-1,8",
			input:      Word(8),
			wantOutput: Words(1),
		},
		{
			code:       "3,9,8,9,10,9,4,9,99,-1,8",
			input:      Word(9),
			wantOutput: Words(0),
		},
		// Using position mode, consider whether the input is less than 8;
		// output 1 (if it is) or 0 (if it is not).
		{
			code:       "3,9,7,9,10,9,4,9,99,-1,8",
			input:      Word(7),
			wantOutput: Words(1),
		},
		{
			code:       "3,9,7,9,10,9,4,9,99,-1,8",
			input:      Word(8),
			wantOutput: Words(0),
		},
		{
			code:       "3,9,7,9,10,9,4,9,99,-1,8",
			input:      Word(9),
			wantOutput: Words(0),
		},
		// Using immediate mode, consider whether the input is equal to 8;
		// output 1 (if it is) or 0 (if it is not).
		{
			code:       "3,3,1108,-1,8,3,4,3,99",
			input:      Word(7),
			wantOutput: Words(0),
		},
		{
			code:       "3,3,1108,-1,8,3,4,3,99",
			input:      Word(8),
			wantOutput: Words(1),
		},
		{
			code:       "3,3,1108,-1,8,3,4,3,99",
			input:      Word(9),
			wantOutput: Words(0),
		},
		// The program will then output 999 if the input value is below 8,
		// output 1000 if the input value is equal to 8,
		// or output 1001 if the input value is greater than 8.
		{
			code:       "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
			input:      Word(-1),
			wantOutput: Words(999),
		},
		{
			code:       "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
			input:      Word(8),
			wantOutput: Words(1000),
		},
		{
			code:       "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
			input:      Word(9),
			wantOutput: Words(1001),
		},
	}

	for i, tc := range cases {
		got := run(asCode(tc.code), tc.input)
		if !reflect.DeepEqual(got.Output, tc.wantOutput) {
			fatalf("test case #%d:\ngot  = %v\nwant = %v\n", i, got.Output, tc.wantOutput)
		}
	}
}
