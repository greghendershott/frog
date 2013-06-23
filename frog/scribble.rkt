#lang rackjure

(require xml
         (only-in html read-html-as-xml)
         "xexpr-map.rkt"
         "util.rkt")

(provide read-scribble-file)

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
                          "--redirect-main" "http://docs.racket-lang.org"
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
  (match (~> (with-input-from-file (build-path dir "frog.html")
               read-html-as-xml)
             second
             xml->xexpr)
    [`(html
       ()
       (head ,_ ...)
       ,(list-no-order
         `(div ([class "maincolumn"])
               (div ([class "main"])
                    (div ([class "versionbox"])
                         (span ([class "versionNoNav"]) ,_))
                    ,xs ...))
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
          [`(blockquote ([class "SCodeFlow"])
                        ,xs ...)
           `(div ([class "SCodeFlow"])
                 ,@xs)]
          [`(img ,(list-no-order `[src ,src] x ...))
           `(img ([src ,(str img-uri "/" src)] ,@x))]
          ;; Scribble @section is rendered as <h3> (and subsection as
          ;; <h4>, and so on). Hoist the headings up a couple levels to
          ;; be consistent with the Markdown format sources.
          [`(h3 ,x ...) `(h1 ,@x)]
          [`(h4 ,x ...) `(h2 ,@x)]
          [`(h5 ,x ...) `(h3 ,@x)]
          [`(h6 ,x ...) `(h4 ,@x)]
          [`(h7 ,x ...) `(h5 ,@x)]
          [x x])))
     x)))
