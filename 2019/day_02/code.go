package main

import (
	"strconv"
	"strings"
)

type Code []int64

func asCode(input string) Code {
	var code Code
	for _, s := range strings.Split(input, ",") {
		num, err := strconv.ParseInt(s, 10, 64)
		fatalIf(err, "parse int")
		code = append(code, num)
	}
	return code
}

func formatCode(c Code) string {
	var ss []string
	for _, num := range c {
		ss = append(ss, strconv.FormatInt(num, 10))
	}
	return strings.Join(ss, " ")
}
