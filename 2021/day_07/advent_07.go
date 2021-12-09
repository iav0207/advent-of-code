package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func main() {
	costIncrement := readParam()
	var pos = readInts()
	sort.Ints(pos)
	idx := map[int]int64{}
	for i := 0; i < len(pos); i++ {
		idx[pos[i]] += 1
	}
	minCost := int64(1e16)
	for dest := pos[0]; dest < pos[len(pos)-1]+1; dest++ {
		cost := int64(0)
		idx_keys := sortedKeys(idx)
		for i := 0; i < len(idx_keys); i++ {
			p := idx_keys[i]
			amount := idx[p]
			length := abs(p - dest)
			eachCost := int64(length * (2 + costIncrement*(length-1)) / 2)
			groupCost := amount * eachCost
			cost += groupCost
		}
		if int64(cost) > minCost {
			fmt.Printf("Destination %d\nCost %d\n", dest, minCost)
			os.Exit(0)
		}
		minCost = cost
	}
	log.Fatalf("No solution found. minCost so far: %d\n", minCost)
}

func readParam() int {
	value, err := strconv.Atoi(os.Args[1])
	if err != nil {
		return 0
	}
	return value
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

func sortedKeys(m map[int]int64) []int {
	keys := make([]int, len(m))
	i := 0
	for k := range m {
		keys[i] = k
		i++
	}
	sort.Ints(keys)
	return keys
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
