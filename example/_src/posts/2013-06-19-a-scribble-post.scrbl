#lang scribble/manual
@; If you want links to online docs, require the module(s):
@(require (for-label racket))

@; The first non-blank text lines must be these 3, the post meta-data:

Title: My Post Title
Date: 2013-06-19T00:00:00
Tags: Racket, blogging

Here is some intro text, above "the jump".

<!-- more -->

Here's some @hyperlink["http://bullshitipsum.com/?paragraphs=2"
"Bullshit Ipsum"].

Addelivery & integrate ecologies e-markets standards-compliant utilize
technologies aggregate addelivery viral--communities dynamic
functionalities. Mindshare engineer viral A-list: cross-platform remix
engage social cross-media social innovate distributed matrix
experiences monetize utilize innovative. Action-items transition
recontextualize sexy Cluetrain envisioneer, "vortals communities
evolve technologies sexy methodologies." Enhance grow compelling
iterate architect matrix plug-and-play reinvent scale, distributed
incentivize, extend.

@section[#:style 'unnumbered]{A Section}

I am some text.

@subsection[#:style 'unnumbered]{A SubSection}

A @racket[codeblock] uses curly braces and @emph{does} retain
comments:

@codeblock{
;; Finds Racket sources in all subdirs
(for ([path (in-directory)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "source file: ~a\n" path)))
(define (foo #:bar bar)
  #t)
}

A @racket[racketblock] uses square brackets and does @emph{not}
retain comments:

@racketblock[
;; Finds Racket sources in all subdirs
(for ([path (in-directory)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "source file: ~a\n" path)))
(define (foo #:bar bar)
  #t)
]
