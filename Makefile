PACKAGENAME=frog
COLLECTS=frog

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf frog/doc

setup:
	raco setup --tidy $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

test:
	raco test -x .
