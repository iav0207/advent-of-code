package main

import (
	"strconv"
	"strings"
)

type Word int64
type Code []Word
type Scanner struct {
    Code Code
    Cursor int
}

func NewScanner(c Code) *Scanner { return &Scanner{Code: c} }

func (s *Scanner) Next() Inst {
    src := s.Code[s.Cursor:]
    instCode := src[0]
    op := Inst(Code([]Word{instCode})).Op()
    src = src[:1+op.ParamCount]
    s.Cursor += len(src)
    return Inst(src)
}

func If(condition bool) Word {
    if condition {
        return True
    }
    return False
}

const (
    True = Word(1)
    False = Word(0)
)

func asCode(input string) Code {
	var code Code
	for _, s := range strings.Split(input, ",") {
		num, err := strconv.ParseInt(s, 10, 64)
		fatalIf(err, "parse int")
		code = append(code, Word(num))
	}
	return code
}

func formatCode(c Code) string {
	var ss []string
	for _, num := range c {
		ss = append(ss, strconv.FormatInt(int64(num), 10))
	}
	return strings.Join(ss, " ")
}

func Words(ws ...Word) []Word { return ws }
