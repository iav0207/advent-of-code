package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"strings"
)

var debugMode bool

func main() {
	debugMode = isDebugMode()
	var line1, line2 string
	_, err := fmt.Scanf("%s\n%s\n", &line1, &line2)
	failIf(err)
	debug(fmt.Sprintf("line1 = %s\nline2 = %s\n", line1, line2))
	var wire1 map[point]member = follow(line1)
	var wire2 map[point]member = follow(line2)
	debug(len(wire1))
	debug(len(wire2))
	var winnerDist int
	center := point{0, 0}
	for point := range wire2 {
		debug(fmt.Sprintf("checking %v", point))
		if _, intersects := wire1[point]; intersects {
			debug("intersects")
			dist := manhattan(center, point)
			if winnerDist == 0 || winnerDist > dist {
				winnerDist = dist
			}
		}
	}
	fmt.Printf("Part 1: %d\n", winnerDist)
}

func follow(wire string) map[point]member {
	points := make(map[point]member)
	curr := point{0, 0}
	for _, section := range strings.Split(wire, ",") {
		debug("section")
		for relLine := parse(section); relLine.length > 0; relLine.length-- {
			curr = curr.shiftByOne(relLine.dir)
			points[curr] = member{}
		}
	}
	return points
}

func manhattan(p, q point) int {
	return int(math.Abs(float64(p.x-q.x))) + int(math.Abs(float64(p.y-q.y)))
}

func parse(section string) relativeLine {
	var (
		dir    string
		length int
	)
	_, err := fmt.Sscanf(section, "%1s%d", &dir, &length)
	failIf(err)
	return relativeLine{dirs[dir], length}
}

type member struct{}
type point struct{ x, y int }

func (p point) shiftByOne(d direction) point {
	return point{p.x + d.dx, p.y + d.dy}
}

type direction struct {
	name   string
	dx, dy int
}

type relativeLine struct {
	dir    direction
	length int
}

var dirs = map[string]direction{
	"U": {"U", 0, 1},
	"L": {"L", -1, 0},
	"D": {"D", 0, -1},
	"R": {"R", 1, 0},
}

func debug(a any) {
	if debugMode {
		fmt.Printf("%v\n", a)
	}
}

func isDebugMode() bool {
	for _, arg := range os.Args[1:] {
		if arg == "-d" {
			return true
		}
	}
	return false
}

func failIf(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
