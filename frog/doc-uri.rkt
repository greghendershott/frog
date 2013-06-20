#lang racket/base

(provide doc-uri)

(require racket/contract
         racket/match
         setup/xref 
         scribble/xref
         scribble/manual-struct)

(module+ test
  (require rackunit))

(define xref (load-collections-xref))

(define/contract (doc-uri sym [mod #f])
  ((symbol?) (symbol?) . ->* . (or/c string?
                                     'none
                                     (listof symbol?)))
  (cond [mod (doc-uri/mod&id mod sym)]
        [else (doc-uri/id sym)]))

;; Starting point: help/help-utils `search-for-exports`.
(define/contract (doc-uri/id sym)
  (symbol? . -> . (or/c string?
                        'none
                        (listof symbol?)))
  (define mods
    (for/list ([entry (in-list (xref-index xref))]
               #:when
               (and (exported-index-desc? (entry-desc entry))
                    (eq? sym (exported-index-desc-name (entry-desc entry)))))
      (car (exported-index-desc-from-libs (entry-desc entry)))))
  ;; If provided by 0 or 1 module, that's simple.
  ;; If multi mods, and one is racket/base or racket, use that.
  ;; Else can't do anything, just return the mods FYI for error result.
  (match mods
    [(list) 'none]
    [(list mod) (doc-uri/mod&id mod sym)]
    [(list-no-order (or 'racket/base) _ ...) (doc-uri/mod&id 'racket/base sym)]
    [(list-no-order (or 'racket) _ ...) (doc-uri/mod&id 'racket sym)]
    [_ mods]))

(define/contract (doc-uri/mod&id mod id)
  (symbol? symbol? . -> . string?) ;throws exn if mod doesn't exist
  (define tag (xref-binding->definition-tag xref (list mod id) #f))
  (define-values (path anchor)
    (xref-tag->path+anchor xref
                           tag
                           #:external-root-url "http://docs.racket-lang.org/"))
  (string-append path "#" anchor))

(module+ test
  (define get-pure-port-uri
    "http://docs.racket-lang.org/net/url.html#(def._((lib._net/url..rkt)._get-pure-port))")
  (check-equal? (doc-uri 'get-pure-port 'net/url) get-pure-port-uri)
  (check-equal? (doc-uri 'get-pure-port) get-pure-port-uri)
  (check-equal? (doc-uri (string->symbol "get-pure-port")) get-pure-port-uri)
  (check-equal? (doc-uri '|NOT DEFINED|) 'none)
  (check-equal? (doc-uri 'rectangle)
                '(2htdp/image htdp/image teachpack/deinprogramm/image
                              slideshow/pict))
  (define printf-uri
    "http://docs.racket-lang.org/reference/Writing.html#(def._((quote._~23~25kernel)._printf))")
  (check-equal? (doc-uri 'printf)
                printf-uri
                "`printf` provided by multi libs, but one is racket/base"))
