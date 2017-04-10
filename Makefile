PACKAGENAME=frog

all: setup

install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGENAME)

setup:
	raco setup --tidy --check-pkg-deps $(PACKAGENAME)

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf frog/doc

check-pkg-deps:
	raco setup -D --check-pkg-deps frog

test:
	raco test -x -p $(PACKAGENAME)

TEST-PROJECT=test-blog
create-build-example:
	mkdir $(TEST-PROJECT) && \
		cd $(TEST-PROJECT) && \
		raco frog --init && \
		raco frog -b && \
		cd .. && \
		rm -rf $(TEST-PROJECT)
