# Frog

<p><a href="http://www.flickr.com/photos/doug88888/4717363945/" title="Happy Green frog by @Doug88888, on Flickr"><img src="http://farm5.staticflickr.com/4070/4717363945_b73afd78a9.jpg" width="300" height="300" alt="Happy Green frog"></a></p>

<p><sub><em><a href="http://www.flickr.com/photos/doug88888/4717363945/">Frog image by @Goug8888</a>, used under Creative Commons license <a href="http://creativecommons.org/licenses/by-nc-sa/2.0/">Attribution-NonCommercial-ShareAlike 2.0 Generic</a>.</em></sub></p>

[![Build Status](https://travis-ci.org/greghendershott/frog.svg?branch=master)](https://travis-ci.org/greghendershott/frog)
[![raco pkg install frog](https://img.shields.io/badge/raco_pkg_install-frog-aa00ff.svg)](http://pkgs.racket-lang.org/package/frog)
[![MIT License](https://img.shields.io/badge/license-MIT-118811.svg)](frog/LICENSE)
[![Documentation](https://img.shields.io/badge/Docs-Documentation-blue.svg)](http://docs.racket-lang.org/frog/index.html)

Frog is a static web site generator written in [Racket][].

You write content in [Markdown][] or [Scribble][]. You generate
files. To deploy, you push them to a GitHub Pages repo (or copy them
to Amazon S3, or whatever).

Posts get a variety of automatic blog features.

You can also create non-post pages.

[Pygments][] handles syntax highlighting for code blocks.

The generated site uses [Bootstrap][], which is [responsive][],
automatically adapting to various screen sizes.

[Full documentation](http://docs.racket-lang.org/frog/index.html).

[Racket]: http://www.racket-lang.org
[Markdown]: http://daringfireball.net/projects/markdown/syntax
[Scribble]: http://docs.racket-lang.org/scribble/index.html
[Pygments]: http://pygments.org/
[Bootstrap]: http://getbootstrap.com/
[responsive]: https://en.wikipedia.org/wiki/Responsive_web_design

# What to expect in the future

TL;DR: This project is in low-maintenance mode.

I have enjoyed working on Frog for many years. In the beginning, it
was intended to be a "just-works", "any color you want, so long as
it's black" simple application.

I envisioned it as an _application_ that happened to be written in
Racket -- which some people might use even if they weren't Racket
programmers. That didn't happen. At the same time, it evolved to be
more configurable. And finally to use a `#lang` for configuration. At
which point it felt more like what is really was: A tool for Racket
programmers to do a static blog.

Eventually I felt even that was too complicated, and [my own blog
should simply be a Makefile driving a few pieces of code inherited
from Frog][explode]. In other words, I no longer use Frog for my own
blog.

As a result, although you're welcome to open issues about new features
or off-label uses, please don't expect much. I expect I will still try
to fix bugs, at least as/when I have time and I believe a fix would
not result in more and/or worse bugs.

As I write this in July 2019, I think the situation has already been
clear from the repo activity for a long time. Even so, I want to be
super up-front about where the project is headed, or not. That way
people can make an informed choice how to spend their limited time and
energy.

Finally a big thank you to people who contributed things over the
years.


[explode]: https://www.greghendershott.com/2019/04/exploding-frog.html

