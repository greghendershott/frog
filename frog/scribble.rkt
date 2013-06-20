#lang rackjure

(require xml
         (only-in html read-html-as-xml))

(provide read-scribble-file)

(define/contract (read-scribble-file path)
  (path? . -> . (listof xexpr?))
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
     (adjust-scribble-html xs)]
    [x
     (displayln "Bad Scribble post:")
     (pretty-print x)
     '()]))

(define (adjust-scribble-html xs)
  (for/list ([x (in-list xs)])
    (match x
      ;; 1. Change code blocks to use <pre> instead of <table>.
      [`(blockquote ([class "SCodeFlow"])
                    (table (,_ ...)
                           (tbody ()
                                  (tr ()
                                      (td () ,xss ...)) ...)))
       `(table ([class "sourcetable"])
               (tbody
                ()
                (tr ()
                    (td ([class "linenos"])
                        (div ([class "linenodiv"])
                             (pre ()
                                  ,@(for/list ([n (in-range (length xss))])
                                      (format "~a\n" (add1 n))))))
                    (td ([class "code"])
                        (div ([class "source"])
                             (pre ()
                                  ,@(simplify&newline xss)))))))]
      ;; 3. Hoist the headings up one level to be consistent with the
      ;; Markdown format sources.
      [`(h2 ,x ...) `(h1 ,@x)]
      [`(h3 ,x ...) `(h2 ,@x)]
      [`(h4 ,x ...) `(h3 ,@x)]
      [`(h5 ,x ...) `(h4 ,@x)]
      [x x])))

(define (simplify&newline xss)
  (append* (for/list ([xs (in-list xss)])
             (append (for/list ([x (in-list xs)])
                       (match x
                         [`(span ([class "hspace"]) nbsp) 'nbsp]
                         [`(span ([class "RktMeta"])) ""] ;noise
                         [_ x]))
                     (list "\n")))))
