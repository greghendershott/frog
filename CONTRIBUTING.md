Contributing to Frog
==================

Interested in improving Frog? Great!

Trying out your new code
----------------------

First, clone or download the Git repository.

The entrypoint to the application is the `main` submodule in `frog/frog` module (as is specified in `frog/info.rkt`). After having introduced some change to the code you may thus take your your new and improved version of Frog for drive by invoking it like so:

	$ racket frog/frog.rkt
	
from the repository root. The options will be the same as for the `raco frog` command Try `-h` to convince yourself.

If you have added any tests (which may be especially useful in the early stages of development when you have not yet hooked them in to the rest of Frog) they can be run using `raco test <file.rkt>` or by invoking the corresponding command in your development environment (such as invoking `racket-test` if your using Emacs' `racket-mode`).

The `example` project
-------------------

Frog ships with an example site in the `example` directory. Generating this site can be a good way to test your code, and if you introduce some new feature adding a new post to that site showcasing your code is an excellent idea.

Testing
------

You are encouraged to add tests covering your new code. In addition it is a good idea to make sure that the existing tests also pass. To run the test suite:

	$ raco test -x -p frog
	
from the repository root (or by specifying some specific file as described above).

Upstreaming your change?
-----------------------

If you have fixed a bug or introduced some new feature it is probably relevant to other users of Frog as well. Submit your change by creating a standard GitHub [pull request](https://github.com/greghendershott/frog/pulls).
