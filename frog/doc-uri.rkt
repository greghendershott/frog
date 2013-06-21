#lang racket/base

(provide doc-uri)

(require racket/contract
         racket/match
         racket/function
         setup/xref 
         scribble/xref
         scribble/manual-struct)

(module+ test
  (require rackunit))

(define xref (load-collections-xref))

;; Starting point: help/help-utils `search-for-exports`.
(define/contract (doc-uri sym)
  (symbol? . -> . (or/c string?
                        'none
                        (listof (cons/c symbol? string?))))
  (define (not-planet x)
    (match x
      [`(planet ,_ ...) #f]
      [_ #t]))
  (define mods
    (filter
     not-planet
     (for/list ([entry (in-list (xref-index xref))]
                #:when
                (and (exported-index-desc? (entry-desc entry))
                     (eq? sym (exported-index-desc-name (entry-desc entry)))))
       (car (exported-index-desc-from-libs (entry-desc entry))))))
  ;; If provided by 0 or 1 module, that's simple.
  ;; If multi mods, and one is racket/base or racket, use that.
  ;; Else return them all, sorted by module name.
  (match mods
    [(list) 'none]
    [(list mod) (doc-uri/mod sym mod)]
    [(list-no-order (or 'racket/base) _ ...) (doc-uri/mod sym 'racket/base)]
    [(list-no-order (or 'racket) _ ...) (doc-uri/mod sym 'racket)]
    [_ (sort (map cons
                  mods
                  (map (curry doc-uri/mod sym) mods))
             symbol<? #:key car)]))

(define (doc-uri/mod id mod)
  ;; (symbol? symbol? . -> . string?)
  ;; throws exn if mod doesn't exist
  (define tag (xref-binding->definition-tag xref (list mod id) #f))
  (cond [tag (define-values (path anchor)
               (xref-tag->path+anchor
                xref
                tag
                #:external-root-url "http://docs.racket-lang.org/"))
             (string-append path "#" anchor)]
        [else (displayln id)
              (displayln mod)
              ""]))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(module+ test
  (define get-pure-port-uri
    "http://docs.racket-lang.org/net/url.html#(def._((lib._net/url..rkt)._get-pure-port))")
  (check-equal? (doc-uri 'get-pure-port) get-pure-port-uri)
  (check-equal? (doc-uri (string->symbol "get-pure-port")) get-pure-port-uri)
  (check-equal? (doc-uri '|NOT DEFINED|) 'none)
  (check-equal? (doc-uri 'rectangle)
                '((2htdp/image . "http://docs.racket-lang.org/teachpack/2htdpimage.html#(def._((lib._2htdp/image..rkt)._rectangle))")
                  (htdp/image . "http://docs.racket-lang.org/teachpack/image.html#(def._((lib._htdp/image..rkt)._rectangle))")
                  (slideshow/pict . "http://docs.racket-lang.org/slideshow/Basic_Pict_Constructors.html#(def._((lib._slideshow/pict..rkt)._rectangle))")
                  (teachpack/deinprogramm/image . "http://docs.racket-lang.org/deinprogramm/image.html#(def._((lib._deinprogramm/image..rkt)._rectangle))"))
                "`rectangle` is in multiple libraries, none racket/base or racket")
  (define printf-uri
    "http://docs.racket-lang.org/reference/Writing.html#(def._((quote._~23~25kernel)._printf))")
  (check-equal? (doc-uri 'printf)
                printf-uri
                "`printf` provided by multi libs, but one is racket/base"))
