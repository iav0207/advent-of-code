outdir = out
cls = Advent16
jarname = advent_16.jar

all:
	make build
	make run

run:
	java -jar $(outdir)/$(jarname)

build: clean init
	kotlinc $(cls).kt -include-runtime -d $(outdir)/$(jarname)

init: clean
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)

