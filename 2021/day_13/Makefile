clsname = Advent13
outdir = out

all:
	make build
	make run

run:
	cd $(outdir) && java $(clsname)

build: clean
	javac --source 17 $(clsname).java -d $(outdir)

clean:
	find . -iname "*.class" -delete

