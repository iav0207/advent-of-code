part1bin = bin/advent_03_01

part1: build
	awk '{$$1=$$1} 1' FS=  input.txt | rs -Tng0 | $(part1bin) # passing transposed input to the c++ program

part2:
	./advent_03_02.py

build: clean init
	g++ advent_03_01.cpp -o $(part1bin) --std=c++11

init:
	mkdir -p bin

clean:
	rm -rf bin

