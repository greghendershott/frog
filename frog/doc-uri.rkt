#lang racket

(provide doc-uri
         sym-mods
         doc-uri/sym-in-mod)

(require racket/contract
         racket/match
         racket/function
         setup/xref 
         scribble/xref
         scribble/manual-struct)

(module+ test
  (require rackunit))

;; This simplified interface returns one doc URI or #f for a given
;; symbol. When a symbol is provided by multiple libs: If one is
;; racket or racket/base, then that doc is used, else #f.
(define/contract (doc-uri sym #:root-uri [root "http://docs.racket-lang.org/"])
  ((symbol?) (#:root-uri (or/c #f string?)) . ->* . (or/c string? #f))
  (define mod
    (match (sym-mods sym)
      [(list) #f]
      [(list m) m]
      [(list-no-order (and (or 'racket/base 'racket) m) _ ...) m]
      [_ #f]))
  (and mod (doc-uri/sym-in-mod sym mod root)))

(module+ test
  (define get-pure-port-uri
    "http://docs.racket-lang.org/net/url.html#(def._((lib._net/url..rkt)._get-pure-port))")
  (check-equal? (doc-uri 'get-pure-port) get-pure-port-uri)
  (check-equal? (doc-uri (string->symbol "get-pure-port")) get-pure-port-uri)
  (check-equal? (doc-uri '|NOT DEFINED|) #f)
  (check-equal? (doc-uri 'rectangle)
                #f
                "`rectangle` is in multiple libraries, none racket/base or racket")
  (define printf-uri
    "http://docs.racket-lang.org/reference/Writing.html#(def._((quote._~23~25kernel)._printf))")
  (check-equal? (doc-uri 'printf)
                printf-uri
                "`printf` provided by multi libs, but one is racket/base"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define xref (load-collections-xref))

;; (symbol? . -> . (listof symbol?))
(define sym-mods
  (let ([cache #f])
    (lambda (sym)
      (unless cache
        (set! cache (make-hasheq))
        (for ([x (in-list (xref-index xref))])
          (match x
            [(entry words content tag (exported-index-desc name from-libs))
             (hash-set! cache name (append (hash-ref cache name '())
                                           (filter symbol? from-libs)))]
            [_ (void)])))
      (hash-ref cache sym #f))))

(module+ test
  (define (symbol<? a b)
    (string<? (symbol->string a) (symbol->string b)))
  (check-equal?
   (sort (sym-mods 'printf) symbol<?)
   '(lang/htdp-advanced lazy plai/gc2/mutator plai/mutator racket/base))
  (check-equal?
   (sort (sym-mods 'rectangle) symbol<?)
   '(2htdp/image htdp/image slideshow/pict teachpack/deinprogramm/image)))

(define/contract (doc-uri/sym-in-mod id mod ext-root-uri)
  (symbol? symbol? (or/c #f string?) . -> . (or/c #f string?)) ;or exn
  (define tag (xref-binding->definition-tag xref (list mod id) #f))
  (and tag
       (let-values ([(path anchor)
                     (xref-tag->path+anchor xref
                                            tag
                                            #:external-root-url ext-root-uri)])
         (format "~a#~a" path anchor))))

(module+ test
  (check-equal?
   (doc-uri/sym-in-mod 'write 'racket "http://docs.racket-lang.org/")
   "http://docs.racket-lang.org/reference/Writing.html#(def._((quote._~23~25kernel)._write))"))
