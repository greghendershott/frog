#lang racket/base

(require json
         net/uri-codec
         net/url
         racket/list
         racket/match
         racket/port
         racket/string
         rackjure/threading
         rackjure/str
         "doc-uri.rkt"
         "html.rkt"
         "params.rkt"
         "pygments.rkt"
         "xexpr-map.rkt")

(provide enhance-body)

(module+ test (require rackunit))

(define (enhance-body xs)
  (~> xs
      syntax-highlight
      add-racket-doc-links
      auto-embed-tweets))

(define (syntax-highlight xs)
  (for/list ([x xs])
    (match x
      [(or `(pre ([class ,brush]) (code () ,(? string? texts) ...))
           `(pre ([class ,brush]) ,(? string? texts) ...))
       (match brush
         [(pregexp "\\s*brush:\\s*(.+?)\\s*$" (list _ lang))
          `(div ([class ,(str "brush: " lang)])
                ,@(pygmentize (apply string-append texts) lang))]
         [_ `(pre ,@texts)])]
      [x x])))

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

(define (add-racket-doc-links xs)
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
                     (if (current-racket-doc-link-prose?)
                         `(code () ,@(->racket-doc-links xs))
                         x)]
                    ;; Only spans from Pygments lexed as Racket
                    [(`((pre . ,_)
                        (div . ,_)
                        (td . ,_)
                        (tr . ,_)
                        (tbody . ,_)
                        (table . ,_)
                        (div ([class "brush: racket"]) . ,_))
                      `(span ([class ,c]) . ,xs))
                     (if (current-racket-doc-link-code?)
                         `(span ([class ,c]) ,@(->racket-doc-links xs))
                         x)]
                    [(_ x) x])))
               x)))

;; This intentionally only works for an <a> element that's nested
;; alone in a <p>. (In Markdown source this means for example an
;; <http://auto-link> alone with blank lines above and below.) Why?
;; The embedded tweet is a block element.
(define (auto-embed-tweets xs)
  (define (do-it xs)
    (for/list ([x xs])
      (match x
        [`(p ,_ ...
             (a ([href ,(pregexp "^https://twitter.com/[^/]+/status/\\d+$"
                                 (list uri))])
                . ,_))
         ;; Note: Although v1.0 API stopped working June 2013,
         ;; /statuses/oembed is an exception. See
         ;; <https://dev.twitter.com/docs/faq#17750>. That's good
         ;; because v1.1 requires authentication, which would
         ;; complicate this (we would sometimes need to launch a
         ;; browser to do an OAuth flow, yada yada yada).
         (define oembed-url
           (string->url (str "https://api.twitter.com/1/statuses/oembed.json?"
                             "url=" (uri-encode uri)
                             "&align=center")))
         (define js (call/input-url oembed-url get-pure-port read-json))
         (define html ('html js))
         (cond [html (~>> (with-input-from-string html read-html-as-xexprs)
                          (append '(div ([class "embed-tweet"]))))]
               [else x])]
        [_ x])))
  (cond [(current-auto-embed-tweets?) (do-it xs)]
        [else xs]))
