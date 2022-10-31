outdir = out
cls = Advent19
jarname = advent_19.jar

part1:
	java -jar $(outdir)/$(jarname)

part2:
	java -jar $(outdir)/$(jarname) 2

debug:
	java -jar $(outdir)/$(jarname) -d

build: clean init
	kotlinc $(cls).kt -include-runtime -d $(outdir)/$(jarname)

init: clean
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)

