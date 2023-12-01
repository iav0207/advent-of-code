outdir = out
src = advent_{{{DAY}}}.cpp
bin = advent_{{{DAY}}}

all:
	make build
	make run

run:
	$(outdir)/$(bin)

debug:
	$(outdir)/$(bin) -d

build: clean init
	g++ $(src) -o $(outdir)/$(bin) --std=c++20

init: clean
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)

