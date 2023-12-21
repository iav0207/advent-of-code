outdir = out
cls = Advent21
jarname = advent_21.jar

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

