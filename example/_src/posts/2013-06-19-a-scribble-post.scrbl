#lang scribble/manual

@; This is a Scribble comment.

@; The first non-blank text lines MUST be these 3, the post meta-data:

Title: A Scribble Post
Date: 2013-06-19T00:00:00
Tags: Racket, blogging

@; If you want links to online docs, `require` the module(s) using
@; `for-label`:

@(require (for-label racket))

The source of this post is Scribble format (not Markdown).

Here is some intro text, above "the jump".

@; The "jump" marker is simply the same text it would be in
@; Markdown, an HTML comment, "more":

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

Here is a @racket[interaction]:

@(require racket/sandbox
          scribble/eval)
@(define my-tr-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits #f])
     (make-evaluator 'typed/racket/base)))
@interaction[#:eval my-tr-evaluator
(: my-sqr (Real -> Real))
(define (my-sqr x)
(* x x))
(my-sqr 42)]

Here's a fancier one:

@interaction[
(require slideshow/pict)
(define rainbow-colors
  '("red" "orange" "yellow" "green" "blue" "purple"))
(for/list ([c rainbow-colors])
   (colorize (filled-rounded-rectangle 20 20) c))
(for/list ([c rainbow-colors]
           [dir (in-cycle '(right left))])
  (standard-fish 25 25 #:color c #:direction dir))
(cc-superimpose
 (cc-superimpose (cloud 100 80 "lightblue")
                 (cloud 90 70 "white"))
 (hc-append 10
  (standard-fish 30 30 #:color "red" #:direction 'right)
  (standard-fish 25 20 #:color "blue" #:direction 'left)))
]

Here's an example of using Pygments to syntax highlight non-Racket
code:

@(require frog/scribble)

@pygment-code[#:lang "js"]{
function foo() {
  return 7;
}
}
