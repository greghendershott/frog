PACKAGE-NAME=frog

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

all: setup

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Note: Also builds docs (if any) and checks deps.
setup:
	raco setup --tidy $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

# Suitable for both day-to-day dev and CI
test:
	raco test -x -p $(PACKAGE-NAME)

######################################################################
# Unique to Frog:
TEST-PROJECT=test-blog
create-build-example:
	mkdir $(TEST-PROJECT)                && \
	cd $(TEST-PROJECT)                   && \
	raco frog --init                     && \
	raco frog --verbose --build          && \
	raco frog --new-markdown "Blah Blah" && \
	raco frog --new-scribble "Blah Blah" && \
	raco frog --verbose --build          && \
	cd ..                                && \
	rm -rf $(TEST-PROJECT)
