clsname = Advent15
outdir = out

all:
	make build
	make run

run:
	cd $(outdir) && scala $(clsname)

build: clean init
	scalac -d $(outdir) -deprecation $(clsname).scala

init:
	mkdir -p $(outdir)

clean:
	rm -rf $(outdir)
	find . -iname "*.class" -delete

