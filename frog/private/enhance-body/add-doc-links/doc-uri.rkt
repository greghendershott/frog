#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/function
         racket/match
         scribble/manual-struct
         scribble/xref
         setup/xref
         syntax/modresolve
         "../../verbosity.rkt")

(provide doc-uri
         sym-mods
         doc-uri/sym-in-mod)

(module+ test
  (require rackunit))

;; This simplified interface returns one doc URI or #f for a given
;; symbol. (When a symbol is provided by _multiple_ libs? If one of
;; the libs is a "special" one -- e.g. racket, racket/base,
;; typed/racket, or typed/racket/base -- then its doc is used, else
;; #f.)
(define doc-uri
  (let ([memoizes (make-hasheq)])
    (lambda (sym #:root-uri [root "http://docs.racket-lang.org/"])
      ;; ((symbol?) (#:root-uri (or/c #f string?)) . ->* . (or/c string? #f))
      (define (lookup sym root)
        (define mod
          (match (sym-mods sym)
            [(list) #f]
            [(list m) m]
            [(? list? ms) (for/or ([m '(racket/base racket
                                        typed/racket/base typed/racket)])
                            (and (member m ms) m))]
            [_ #f]))
        (and mod
             (main-distribution? mod) ;don't make 404 links to www.r-l.org
             (doc-uri/sym-in-mod sym mod root)))
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

;; Is module part of the Racket main distribution? Warning: This is
;; only semi reliable as of 6.0, and not reliable at all prior to that
;; (always returns #t).
(define (main-distribution? mod)
  (and (symbol? mod) ;not e.g. "/x/y" or "foo.rkt"
       (match (path->pkg (resolve-module-path mod #f))
         [(or #f                  ;what `racket` returns in 6.01
              "base" 'core        ;what `racket` is likely to return (?)
              "typed-racket-lib") ;special case
          mod]
         [_ #f])))

(module+ test
  (unless (string<? (version) "6")
    (check-equal? (main-distribution? 'threading) #f)
    (check-equal? (main-distribution? "/x/y") #f))
  (check-equal? (main-distribution? 'racket) 'racket)
  (check-equal? (main-distribution? 'racket/contract) 'racket/contract)
  (check-equal? (main-distribution? 'typed/racket) 'typed/racket))

;; On Racket 6.0+, path->pkg returns the pkg for a path. Before that,
;; doesn't exist, so use a function that always returns #f (to mimic
;; what path->pkg does as of 6.01 when given a main distribution
;; collection). Kludge on top of a kludge.
(define path->pkg
  (with-handlers ([exn:fail? (λ _ (λ (_) #f))])
    (dynamic-require 'pkg/path 'path->pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define xref (load-collections-xref))

(define sym-mods
  ;; (symbol? . -> . (listof symbol?))
  (let ([cache #f])
    (lambda (sym)
      (unless cache
        (set! cache (make-hasheq))
        (define older-racket? (string<=? (version) "6.0.1"))
        (cond [older-racket?
               (prn0 "Building Racket documentation cache -- MUCH faster in Racket 6.0.1+ ...")]
              [else
               (prn2 "Building Racket documentation cache...")])
        (for ([x (in-list (xref-index xref))])
          (match x
            [(entry words content tag (exported-index-desc name from-libs))
             (hash-set! cache name (append (hash-ref cache name '())
                                           (filter symbol? from-libs)))]
            [_ (void)]))
        (when older-racket?
          (prn0 "...~a items" (hash-count cache))))
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
