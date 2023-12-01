outdir = out
src = advent_01.cpp
bin = advent_01

all:
	make build
	make run

run:
	$(outdir)/$(bin)

debug:
	$(outdir)/$(bin) -d

build: clean init
	g++ --std=c++20 -o $(outdir)/$(bin) -- $(src)

init: clean
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)

