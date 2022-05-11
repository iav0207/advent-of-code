outdir = out
cls = Advent14
jarname = advent_14.jar

all:
	make build
	make run

run:
	java -jar $(outdir)/$(jarname)

debug:
	java -jar $(outdir)/$(jarname) -d

build: clean init
	kotlinc $(cls).kt -include-runtime -d $(outdir)/$(jarname)

init: clean
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)

