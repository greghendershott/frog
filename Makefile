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
	raco setup --tidy --avoid-main $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

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


## Unique to this project:

# Extra sanity checks that you could run locally but normally would
# let CI run for you.
ci-extra-checks: build-example init-build

# Build the example project. Check that an expected URI exists in
# sitemap.txt and is Unicode NFD normalized and percent-encoded.
build-example:
	cd example && raco frog --verbose --clean --build
	grep 'http://www.example.com/2017/05/la-biblioteca-esta%CC%81-en-el-esto%CC%81mago-de-godzilla.html' example/sitemap.txt

# Exercise raco frog --init
TEST-PROJECT=test-blog
init-build:
	mkdir $(TEST-PROJECT)                 && \
	cd $(TEST-PROJECT)                    && \
	raco frog --init                      && \
	raco frog --verbose --build           && \
	raco frog --new-markdown "Hey λ está" && \
	raco frog --new-scribble "Hey λ allí" && \
	raco frog --verbose --build           && \
	cd ..                                 && \
	rm -rf $(TEST-PROJECT)
