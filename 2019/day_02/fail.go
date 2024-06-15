package main

import "log"

func fatalIf(err error, msg string) {
    if err != nil {
        log.Fatalf("%s: %v", msg, err)
    }
}

func fatalf(f string, a ...any) {
    log.Fatalf(f, a...)
}
