#lang rackjure/base

(require json
         net/uri-codec
         net/url
         racket/list
         racket/match
         racket/port
         racket/string
         rackjure/str
         rackjure/threading
         "doc-uri.rkt"
         "html.rkt"
         "params.rkt"
         "pygments.rkt"
         "xexpr-map.rkt"
         "verbosity.rkt"
         "paths.rkt"
         "responsive-images.rkt")

(provide enhance-body)

(module+ test (require rackunit))

(define (enhance-body xs)
  (~> xs
      responsive-images
      syntax-highlight
      add-racket-doc-links
      auto-embed-tweets))

(define responsive-images
  (let ([magick-notice-displayed? #f])
    (λ (xs)
      (define (remote-host url)
        (url-host (string->url url)))
      (define (do-it xs)
        (for/list ([x xs])
          (match x
            [`(div ([class ,classes])
                   (img ,(list-no-order `[src ,url] attrs ...))
                   ,content ...)
             #:when (and (regexp-match #px"\\bfigure\\b" classes)
                         (not (remote-host url)))
             (let ([sizes-attr (assq 'sizes attrs)])
               `(div ([class ,classes])
                     (img ([class "img-responsive"] ; Add Bootstrap class
                           ,@(make-responsive url (cond [sizes-attr => second]
                                                        [#t #f]))
                           ,@(if sizes-attr
                                 (remove sizes-attr attrs)
                                 attrs)))
                     ,@content))
             ]
            ;; xexpr-map?
            [`(p () (img ,(list-no-order `[src ,url] `[class ,classes] attrs ...)))
             #:when (and (regexp-match #px"\\bimg-responsive\\b" classes)
                         (not (remote-host url)))
             `(p () (img ([class ,classes]
                          ,@(make-responsive url #f) ; TODO honor custom sizes?
                          ,@attrs)))]
            [x x])))
      (cond [(current-responsive-images?)
             (if magick-available?
                 (do-it xs)
                 (begin
                   (unless magick-notice-displayed?
                     (prn1 "ImageMagick not found. Omitting img srcset attributes.")
                     (set! magick-notice-displayed? #t))
                   xs))]
            [else xs]))))


(module+ test
  (parameterize ([top example]
                 [current-responsive-images? #t]
                 [current-image-output-dir "resized"]
                 [current-image-sizes-attr #f]
                 [current-image-sizes '(320 600 1200)]
                 [current-image-default-size 600]
                 [current-verbosity 0])
    (test-equal? "Remote images"
                 (responsive-images
                  '((div ((class "figure")) (img ((src "//somehost.com/img/file.jpg"))))))
                 ;; Don't resize remote images. Or should we fetch it and resize it?
                 '((div ((class "figure")) (img ((src "//somehost.com/img/file.jpg"))))))
    (when magick-available?
      (test-equal? "Element-specific custom sizes attribute"
                   (responsive-images
                    '((div ([class "figure"])
                           (img ([src "/img/1x1.gif"]
                                 [sizes "some-custom-size-spec"])))))
                   '((div ((class "figure"))
                          (img ([class "img-responsive"]
                                [src "/img/1x1.gif"]
                                [srcset "/img/1x1.gif 2w"]
                                [sizes "some-custom-size-spec"])))))
      (test-equal? "Img with img-responsive class inside p tag"
                   (responsive-images
                    '((p () (img ([src "/img/1x1.gif"]
                                  [alt ""]
                                  [class "img-responsive among-others"]
                                  [foo-attr "bar"])))))
                   '((p () (img ([class "img-responsive among-others"]
                                 [src "/img/1x1.gif"]
                                 [srcset "/img/1x1.gif 2w"]
                                 [sizes "(max-width: 2px) 100vw, 2px"]
                                 [alt ""]
                                 [foo-attr "bar"])))))
      (test-equal? "Image bigger than maximum size"
                   (responsive-images
                    '((div ([class "figure pull-right"])
                           (img ([src "/img/1300px-image.gif"] (alt "")))
                           (p ([class "caption"]) "some text"))))
                   `((div ((class "figure pull-right"))
                          (img ([class "img-responsive"]
                                [src "/img/resized/600/1300px-image.gif"]
                                [srcset
                                 ,(string-join
                                   (for/list ([s (current-image-sizes)])
                                     (format "/img/resized/~a/1300px-image.gif ~aw" s s))
                                   ", ")]
                                [sizes "(max-width: 1300px) 100vw, 1300px"]
                                (alt "")))
                          (p ((class "caption")) "some text"))))
      (test-equal? "Image smaller than biggest size but bigger than smallest size"
                   (responsive-images
                    '((div ((class "figure"))
                           (img ((src "/img/800px-image.gif") (alt "")))
                           (p ((class "caption")) "some text"))))
                   `((div ((class "figure"))
                          (img ([class "img-responsive"]
                                (src ,(format "/img/resized/~a/800px-image.gif"
                                              (current-image-default-size)))
                                (srcset
                                 ,(string-append
                                   (string-join
                                    (for/list ([s '(320 600)])
                                      (format "/img/resized/~a/800px-image.gif ~aw" s s))
                                    ", ")
                                  ", /img/800px-image.gif 800w"))
                                (sizes "(max-width: 800px) 100vw, 800px")
                                (alt "")))
                          (p ((class "caption")) "some text"))))
      (test-equal? "Image equal to a one of the sizes specified"
                   (responsive-images
                    '((div ((class "figure"))
                           (img ((src "/img/600px-image.gif") (alt "")))
                           (p ((class "caption")) "some text"))))
                   '((div ((class "figure"))
                          (img ([class "img-responsive"]
                                (src "/img/600px-image.gif")
                                (srcset "/img/resized/320/600px-image.gif 320w, /img/600px-image.gif 600w")
                                (sizes "(max-width: 600px) 100vw, 600px")
                                (alt "")))
                          (p ((class "caption")) "some text"))))
      (test-equal? "Image smaller than smallest size"
                   (responsive-images
                    '((div ((class "figure"))
                           (img ((src "/img/1x1.gif") (alt ""))) ; Tiny image
                           (p ((class "caption")) "some text"))))
                   '((div ((class "figure"))
                          (img ([class "img-responsive"]
                                (src "/img/1x1.gif")
                                (srcset "/img/1x1.gif 2w")
                                (sizes "(max-width: 2px) 100vw, 2px")
                                (alt "")))
                          (p ((class "caption")) "some text"))))
      (wait-resize-images)
      (clean-resized-images))))

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
                        ,_ ... ;varies: line numbers?
                        (div ([class "brush: racket"]) . ,_))
                      `(span ([class ,(and c (not "c1"))]) . ,xs))
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
                             "&align=center"
                             (if (current-embed-tweet-parents?)
                                 ""
                                 "&hide_thread=true"))))
         (define js (call/input-url oembed-url get-pure-port read-json))
         (define html ('html js))

         (cond [html (~>> (with-input-from-string html read-html-as-xexprs)
                          (append '(div ([class "embed-tweet"]))))]
               [else x])]
        [_ x])))
  (cond [(current-auto-embed-tweets?) (do-it xs)]
        [else xs]))
