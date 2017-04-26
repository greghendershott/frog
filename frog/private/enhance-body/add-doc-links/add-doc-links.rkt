#lang racket/base

(require json
         net/uri-codec
         net/url
         racket/list
         racket/match
         racket/port
         racket/string
         rackjure/str
         rackjure/threading
         "../../xexpr-map.rkt"
         "doc-uri.rkt")

(provide add-racket-doc-links)

(define (->racket-doc-links xs)
  (define (not-empty-string s)
    (not (and (string? s)
              (string=? s ""))))
  (define a-string
    (string-join (for/list ([x (in-list xs)])
                   (match x
                     [(? integer?) (make-string 1 (integer->char x))]
                     [(? string?) x]
                     [_ ""]))
                 ""))
  (filter
   not-empty-string
   (add-between
    (for/list ([s (in-list (regexp-split #rx" " a-string))])
      (match (doc-uri (string->symbol s))
        [(? string? uri) `(a ([href ,uri] [style "color: inherit"]) ,s)]
        [_ s]))
    " ")))

(module+ test
  (require rackunit)
  (check-equal?
   (->racket-doc-links '("printf "))
   '((a ((href "http://docs.racket-lang.org/reference/Writing.html#(def._((quote._~23~25kernel)._printf))") (style "color: inherit")) "printf")
     " "))
  (check-equal?
   (->racket-doc-links '("symbol-" ">" "string"))
   '((a ([href "http://docs.racket-lang.org/reference/symbols.html#(def._((quote._~23~25kernel)._symbol-~3estring))"]
         [style "color: inherit"])
        "symbol->string")))
  (check-equal?
   (->racket-doc-links '("printf displayln"))
   '((a ([href "http://docs.racket-lang.org/reference/Writing.html#(def._((quote._~23~25kernel)._printf))"]
         [style "color: inherit"])
        "printf")
     " "
     (a ([href "http://docs.racket-lang.org/reference/Writing.html#(def._((lib._racket/private/misc..rkt)._displayln))"]
         [style "color: inherit"])
        "displayln"))))

(define (add-racket-doc-links xs #:code? code? #:prose? prose?)
  (for/list ([x (in-list xs)])
    (xexpr-map (lambda (x parents)
                 ;; Not necessarily (tag () "string"). For example it
                 ;; won't be (tag () "number->symbol"), it will be
                 ;; (tag () "number-" ">" "symbol").
                 (list
                  (match* (parents x)
                    ;; Markdown `symbol`[racket] becomes xexpr like
                    ;; (code ([class "brush: racket"]) "symbol")
                    [(_
                      `(code ([class "brush: racket"]) . ,xs))
                     (if prose?
                         `(code () ,@(->racket-doc-links xs))
                         x)]
                    ;; Only spans from Pygments lexed as Racket
                    [(`((pre . ,_)
                        (div . ,_)
                        ,_ ... ;varies: line numbers?
                        (div ([class "brush: racket"]) . ,_))
                      `(span ([class ,(and c (not "c1"))]) . ,xs))
                     (if code?
                         `(span ([class ,c]) ,@(->racket-doc-links xs))
                         x)]
                    [(_ x) x])))
               x)))
