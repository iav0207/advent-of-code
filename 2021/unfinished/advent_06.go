package main

import "fmt"
import "os"
import "strconv"
import "bufio"
import "log"
import "encoding/json"

func main() {
    var t [9] int
    var n int
    n, _ = strconv.Atoi(os.Args[1])
    input := readInts()
    for _, timer := range input {
        t[timer] += 1
    }

    for i := 0; i < n; i++ {
        var fertile = i % 9
        var reset_to = (i + 6) % 9
        t[reset_to] += t[fertile]
    }

    var sum = 0
    for _, population := range t { sum += population }

    fmt.Print(sum)
}

func readInts() []int {
	reader := bufio.NewReader(os.Stdin)
	str, _ := reader.ReadString('\n')

	var ints []int
	err := json.Unmarshal([]byte(fmt.Sprintf("[%s]", str)), &ints)
	if err != nil {
		log.Fatalf("FATAL: %s", err)
	}
	return ints
}


