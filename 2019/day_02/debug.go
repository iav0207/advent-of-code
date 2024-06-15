package main

import (
"fmt"
"os"
)

var debugMode = false

func init() {
    for _, arg := range os.Args[1:] {
        if arg == "-d" || arg == "--debug" {
            debugMode = true
            break
        }
    }
}

func debugln(anys ...any) {
    if debugMode {
        fmt.Println(anys...)
    }
}

func debugf(f string, a ...any) {
    if debugMode {
        fmt.Printf(f, a...)
    }
}

