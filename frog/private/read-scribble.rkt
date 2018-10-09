#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/file
         racket/match
         racket/pretty
         rackjure/str
         rackjure/threading
         (only-in xml xexpr?)
         "html.rkt"
         "util.rkt"
         "xexpr-map.rkt")

(provide read-scribble-file)

(module+ test
  (require rackunit))

(define/contract (read-scribble-file path
                                     #:img-local-path img-dir
                                     #:img-uri-prefix img-uri)
  (path? #:img-local-path path? #:img-uri-prefix string? . -> . (listof xexpr?))
  ;; This way of running Scribble is cribbed from Ryan Culpepper's
  ;; Scriblogify:
  (define dir (path->string (make-temporary-file "frog~a" 'directory)))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-command-line-arguments
                  (vector "--quiet"
                          "--html"
                          "--dest" dir
                          "--dest-name" "frog.html"
                          "--redirect-main" "https://docs.racket-lang.org"
                          "++xref-in" "setup/xref" "load-collections-xref"
                          (path->string path))])
    (dynamic-require 'scribble/run #f))
  ;; Move any .PNG or .GIF or .SVG files from dir to img-dir
  (define (image-file? p)
    (match (path->string p)
      [(pregexp "(?i:\\.((png)|(gif)|(svg)))$") #t]
      [_ #f]))
  (for ([from (in-list (find-files image-file? dir))])
    (define-values (base name _) (split-path from))
    (define to (build-path img-dir name))
    (copy-file* from to #t))
  ;; Extract the part we care about -- the elements in the "main" div
  ;; after the "versionbox" div.  (The `match` might be too fragile
  ;; way to do this.)
  (match (~> (build-path dir "frog.html")
             (with-input-from-file read-html-as-xexprs)
             cadr)
    ; HTML produced from #scribble/manual
    [`(html
       ()
       (head . ,_)
       ,(list-no-order
         `(div ([class "maincolumn"])
               (div ([class "main"])
                    (div ([class "versionbox"])
                         (span ([class "versionNoNav"]) ,_))
                    . ,xs))
         _ ...))
     (adjust-scribble-html xs img-uri)]
    ; HTML produced from #scribble/base
    [`(html
       ()
       (head . ,_)
       ,(list-no-order
         `(div ([class "maincolumn"])
               (div ([class "main"])
                    . ,xs))
         _ ...))
     (adjust-scribble-html xs img-uri)]
    [x
     (displayln "Bad Scribble post:")
     (pretty-print x)
     '()]))

(define (adjust-scribble-html xs img-uri)
  (for/list ([x (in-list xs)])
    (xexpr-map
     (lambda (x _)
       (list
        (match x
          [`(blockquote ([class "SCodeFlow"]) . ,xs)
           `(div ([class "SCodeFlow"]) ,@xs)]
          [`(img ,(list-no-order `[src ,src] x ...))
           `(img ([src ,(str img-uri "/" src)] ,@x))]
          ;; Scribble @title is rendered as <h2>, @section as <h3>,
          ;; and @subsection as <h4>, and so on. Hoist the headings up
          ;; to be consistent with the Markdown format sources.
          [`(h2 . ,x) `(h1 ,@x)]   ;elsewhere we special-case 1st h1
          [`(h3 . ,x) `(h1 ,@x)]
          [`(h4 . ,x) `(h2 ,@x)]
          [`(h5 . ,x) `(h3 ,@x)]
          [`(h6 . ,x) `(h4 ,@x)]
          [`(h7 . ,x) `(h5 ,@x)]
          [`(p () "<" "!" ndash " more " ndash ">") `(!HTML-COMMENT () "more")]
          [x x])))
     x)))

(module+ test
  (let ([path (make-temporary-file)]
        [s #<<EOF
#lang scribble/manual
@title{The Post's Title}
@section{Section 1}
Here is some text.

<!-- more -->

Below the fold.
EOF
])
    (with-output-to-file path #:exists 'replace (λ () (display s)))
    (check-equal?
     (read-scribble-file path
                         #:img-local-path (find-system-path 'temp-dir)
                         #:img-uri-prefix "/")
     '((h1 () (a ((name "(part._.The_.Post_s_.Title)"))) "The Post" rsquo "s Title")
      (h1 () "1" (tt () nbsp) (a ((name "(part._.Section_1)"))) "Section 1")
      (p () "Here is some text.")
      (!HTML-COMMENT () "more")
      (p () "Below the fold.")))
    (delete-file path))
  ;; regression test for https://github.com/greghendershott/frog/issues/75
  (let ([path (make-temporary-file)]
        [s #<<EOF
#lang scribble/manual
@hyperlink["https://aur.archlinux.org/packages/?SeB=m&K=bluephoenix47" "Aur"]
EOF
])
    (with-output-to-file path #:exists 'replace (λ () (display s)))
    (check-equal?
     (read-scribble-file path
                         #:img-local-path (find-system-path 'temp-dir)
                         #:img-uri-prefix "/")
     '((p ()
          (a ((href "https://aur.archlinux.org/packages/?SeB=m&K=bluephoenix47")) "Aur"))))
    (delete-file path)))
