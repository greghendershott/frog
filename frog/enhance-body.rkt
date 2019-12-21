#lang at-exp racket/base

(require racket/require
         json
         (multi-in net (uri-codec url))
         (multi-in racket (contract/base format list match string port))
         threading
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual)
         xml/xexpr
         (multi-in "private" ("define-doc.rkt"
                              "html.rkt"
                              "xexpr-map.rkt"))
         (multi-in "private/enhance-body" ("add-doc-links/doc-uri.rkt"
                                           "syntax-highlight/pygments.rkt")))

(define/doc (syntax-highlight
             [x-expressions (listof xexpr/c)]
             [#:python-executable python-executable path-string? (if (eq? (system-type) 'windows)
                                                                     "python.exe"
                                                                     "python")]
             [#:line-numbers? line-numbers? boolean? #t]
             [#:css-class css-class string? "source"]
             (listof xexpr/c))
  @{Use Pygments to highlight markdown code blocks.
    @margin-note*{Tip: In Scribble format sources, you can use
                  @racket[pygment-code].}

    The value of @racket[python-executable] is given to
    @racket[find-executable-path], so the default @racket["python"]
    will work if the version of Python that Pygments wants is on the
    path.}
  (let recur ([xs x-expressions])
    (for/list ([x xs])
      (match x
        [(or `(pre ([class ,brush]) (code () ,(? string? texts) ...))
             `(pre ([class ,brush]) ,(? string? texts) ...))
         (match brush
           [(pregexp "\\s*brush:\\s*(.+?)\\s*$" (list _ lang))
            `(div ([class ,(~a "brush: " lang)])
              ,@(pygmentize (apply string-append texts) lang
                            #:python-executable python-executable
                            #:line-numbers? line-numbers?
                            #:css-class css-class))]
           [_ `(pre ,@texts)])]
        ;; Check child elements, too. For example as in issue #217
        ;; could be (ol () (li () (pre () ___))).
        [(list* (? symbol? tag) (? list? attributes) elements)
         (list* tag attributes (recur elements))]
        [x x]))))

;; This intentionally only works for an <a> element that's nested
;; alone in a <p>. (In Markdown source this means for example an
;; <http://auto-link> alone with blank lines above and below.) Why?
;; The embedded tweet is a block element.
(define/doc (auto-embed-tweets
             [xs (listof xexpr/c)]
             [#:parents? parents? boolean?]
             (listof xexpr/c))
  @{Replace links to tweets with embedded tweets. In markdown, must be
    auto-links alone in a paragraph (blank lines above and below), for
    example:

    @verbatim{<https://twitter.com/racketlang/status/332176422003163138>}

    When @racket[parents?] is true also embeds parent tweets.}
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
         (string->url
          (~a "https://api.twitter.com/1/statuses/oembed.json?"
              "url=" (uri-encode uri)
              "&align=center"
              (if parents?
                  ""
                  "&hide_thread=true"))))
       (define js (call/input-url oembed-url get-pure-port read-json))
       (match (hash-ref js 'html)
         [html (~>> (with-input-from-string html read-html-as-xexprs)
                    (append '(div ([class "embed-tweet"]))))]
         [_ x])]
      [_ x])))

(define/doc (add-racket-doc-links
             [xs (listof xexpr/c)]
             [#:code? code? boolean?]
             [#:prose? prose? boolean?]
             (listof xexpr/c))
  @{When @racket[code?] is true, try to automatically link to Racket
    documentation from symbols in @litchar{```racket} markdown fenced
    code blocks.

    When @racket[prose?] is true, try to automatically link to Racket
    documentation from symbols in markdown of the form
    @litchar{`symbol`[racket]}? i.e. This is similar to the
    @tt|{@racket[]}| form in Scribble.}
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
                        ,_ ...          ;varies: line numbers?
                        (div ([class "brush: racket"]) . ,_))
                      `(span ([class ,(and c (not "c1"))]) . ,xs))
                     (if code?
                         `(span ([class ,c]) ,@(->racket-doc-links xs))
                         x)]
                    [(_ x) x])))
               x)))

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
