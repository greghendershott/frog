#lang racket

(provide doc-uri
         sym-mods
         doc-uri/sym-in-mod)

(require setup/xref
         scribble/xref
         scribble/manual-struct
         "verbosity.rkt")

(module+ test
  (require rackunit))

;; This simplified interface returns one doc URI or #f for a given
;; symbol. When a symbol is provided by multiple libs: If one of the
;; libs is racket or racket/base then its doc is used, else #f.
(define doc-uri
  (let ([memoizes (make-hasheq)])
    (lambda (sym #:root-uri [root "http://docs.racket-lang.org/"])
      ;; ((symbol?) (#:root-uri (or/c #f string?)) . ->* . (or/c string? #f))
      (define (lookup sym root)
        (define mod
          (match (sym-mods sym)
            [(list) #f]
            [(list m) m]
            [(list-no-order (and (or 'racket/base 'racket) m) _ ...) m]
            [_ #f]))
        (and mod (doc-uri/sym-in-mod sym mod root)))
      (define k (string->symbol (format "~a,~a" sym root)))
      (hash-ref memoizes k (lambda ()
                             (define v (lookup sym root))
                             (hash-set! memoizes k v)
                             v)))))

(module+ test
  (check-equal? (doc-uri 'get-pure-port #:root-uri "")
                "net/url.html#(def._((lib._net/url..rkt)._get-pure-port))")
  (check-equal? (doc-uri '|NOT DEFINED|)
                #f)
  (check-equal?
   (doc-uri 'rectangle)
   #f
   "`rectangle` is in multiple libraries, none racket/base or racket")
  (check-equal?
   (doc-uri 'printf #:root-uri "")
   "reference/Writing.html#(def._((quote._~23~25kernel)._printf))"
   "`printf` provided by multi libs, but one is racket/base"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define xref (load-collections-xref))

(define sym-mods
  ;; (symbol? . -> . (listof symbol?))
  (let ([cache #f])
    (lambda (sym)
      (unless cache
        (set! cache (make-hasheq))
        (prn0 "Building Racket documentation cache...")
        (for ([x (in-list (xref-index xref))])
          (match x
            [(entry words content tag (exported-index-desc name from-libs))
             (hash-set! cache name (append (hash-ref cache name '())
                                           (filter symbol? from-libs)))]
            [_ (void)]))
        (prn0 "...~a items" (hash-count cache)))
      (hash-ref cache sym #f))))

(module+ test
  (define (symbol<? a b)
    (string<? (symbol->string a) (symbol->string b)))
  (check-equal? (sort (sym-mods 'printf) symbol<?)
                '(lang/htdp-advanced lazy plai/gc2/mutator plai/mutator
                                     racket racket/base)
                "this test may not be portable"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (doc-uri/sym-in-mod id mod ext-root-uri)
  (symbol? symbol? (or/c #f string?) . -> . (or/c #f string?)) ;or exn
  (define tag (xref-binding->definition-tag xref (list mod id) #f))
  (with-handlers ([exn:fail? (const #f)]) ;in case path has 'up
    (and tag
         (let-values ([(path anchor)
                       (xref-tag->path+anchor xref
                                              tag
                                              #:external-root-url ext-root-uri)])
           (format "~a#~a" path anchor)))))

(module+ test
  (check-equal?
   (doc-uri/sym-in-mod 'write 'racket "/")
   "/reference/Writing.html#(def._((quote._~23~25kernel)._write))"))
