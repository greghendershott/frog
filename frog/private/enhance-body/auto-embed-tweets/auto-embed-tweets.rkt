#lang racket/base

(require json
         net/uri-codec
         net/url
         racket/match
         racket/port
         rackjure/str
         rackjure/threading
         "../../html.rkt")

(provide auto-embed-tweets)

;; This intentionally only works for an <a> element that's nested
;; alone in a <p>. (In Markdown source this means for example an
;; <http://auto-link> alone with blank lines above and below.) Why?
;; The embedded tweet is a block element.
(define (auto-embed-tweets xs #:parents? parents?)
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
          (str "https://api.twitter.com/1/statuses/oembed.json?"
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
