outdir = out
cls = Advent11
jarname = advent_11.jar

part = 1

all:
	make build
	make run part=1 < input.txt
	make run part=2 < input.txt

run:
	java -jar $(outdir)/$(jarname) $(part)

build: clean init
	kotlinc $(cls).kt -include-runtime -d $(outdir)/$(jarname)

init: clean
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)

