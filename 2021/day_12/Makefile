part=2 # override with an argument if needed

fname = advent_12
src = $(fname).ts
outdir = out
outfile = $(outdir)/$(fname).js

all:
	ts-node $(src) $(part)

run:
	node ./$(outfile) $(part)

build: clean init
	tsc --outDir $(outdir) --downlevelIteration $(src)
	chmod +x $(outfile)

init:
	npm i --save-dev @types/node

clean:
	rm -rf $(outdir)

