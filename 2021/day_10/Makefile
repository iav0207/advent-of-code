all:
	make build
	make run

run:
	bin/advent_10

build: clean init
	gcc advent_10.c stack.c -Wall -g \
		-o bin/advent_10

init:
	mkdir -p bin

clean:
	rm -rf bin

