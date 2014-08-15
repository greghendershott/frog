#lang racket/base

(require (only-in markdown xexpr->string)
         net/uri-codec
         racket/date
         racket/function
         racket/list
         racket/match
         racket/string
         rackjure/threading
         rackjure/str
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "take.rkt"
         "util.rkt"
         "verbosity.rkt")

(provide atom-feed-uri
         write-atom-feed
         rss-feed-uri
         write-rss-feed)

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom-feed-uri tag)
  (str "/feeds/" tag ".atom.xml"))

(define (write-atom-feed xs title tag of-uri-path file)
  (prn1 "Generating ~a" (abs->rel/www file))
  (define updated
    (match xs
      ['() "N/A"]
      [_   (str (post-date (first xs)) "Z")])) ;; lie: not nec. UTC
  (~>
   `(feed
     ([xmlns "http://www.w3.org/2005/Atom"]
      [xml:lang "en"])
     (title ([type "text"]) ,(str (current-title) ": " title))
     (link ([rel "self"]
            [href ,(full-uri (abs->rel/www file))]))
     (link ([href ,(full-uri of-uri-path)]))
     (id () ,(str "urn:"
                 (our-encode (current-scheme/host))
                 ":"
                 (our-encode of-uri-path)))
     ;; (etag () ???)
     (updated () ,updated)
     ,@(map (curry post->atom-feed-entry-xexpr tag)
            (take<= xs (current-max-feed-items))))
   xexpr->string
   (list "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
   reverse
   string-join
   string->bytes/utf-8
   (display-to-file* file #:exists 'replace)))

(define (post->atom-feed-entry-xexpr tag x)
  (match-define (post title src-path modtime dest-path uri-path date older newer tags blurb more? body) x)
  (define item-uri (full-uri/decorated uri-path #:source tag #:medium "Atom"))
  `(entry
    ()
    (title ([type "text"]) ,title)
    (link ([rel "alternate"]
           [href ,item-uri]))
    (id () ,(str "urn:"
                 (our-encode (current-scheme/host))
                 ":"
                 (our-encode uri-path)))
    (published () ,(str date "Z")) ;; lie: not necessarily UTC
    (updated () ,(str date "Z"))   ;; lie: not necessarily UTC
    (author (name ,(current-author)))
    (content
     ([type "html"])
     "<html>"
     ,(cond [(current-feed-full?) body] ;don't enhance-body
            [more? (string-append blurb
                                  (xexpr->string `(a ([href ,item-uri])
                                                   (em "More" hellip))))]
            [else blurb])
     "</html>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rss-feed-uri tag)
  (str "/feeds/" tag ".rss.xml"))

(define (write-rss-feed xs title tag of-uri-path file)
  (prn1 "Generating ~a" (abs->rel/www file))
  (define updated
    (match xs
      ['() "N/A"]
      [_   (~> xs first post-date rfc-8601->822)]))
  (~>
   `(rss
     ([version "2.0"])
     (channel
      (title ,(str (current-title) ": " title))
      (description ,(str (current-title) ": " title))
      (link ,(full-uri of-uri-path))
      (lastBuildDate () ,updated)
      (pubDate ,updated)
      (ttl "1800")
      ,@(map (curry post->rss-feed-entry-xexpr tag)
             (take<= xs (current-max-feed-items)))))
   xexpr->string
   (list "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
   reverse
   string-join
   string->bytes/utf-8
   (display-to-file* file #:exists 'replace)))

(define (post->rss-feed-entry-xexpr tag x)
  (match-define (post title src-path modtime dest-path uri-path date older newer tags blurb more? body) x)
  (define item-uri (full-uri/decorated uri-path #:source tag #:medium "RSS"))
  `(item
    ()
    (title ,title)
    (link ,item-uri)
    (guid () ,(str "urn:"
                   (our-encode (current-scheme/host))
                   ":"
                   (our-encode uri-path)))
    (pubDate () ,(~> date rfc-8601->822))
    (description
     "<html>"
     ,(cond [(current-feed-full?) body] ;don't enhance-body
            [more? (string-append blurb
                                  (xexpr->string `(a ([href ,item-uri])
                                                   (em "More" hellip))))]
            [else blurb])
     "</html>")))

(define (rfc-8601->822 s)
  (match s
    [(pregexp "(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})"
              (list _ year month day hour minute second))
     (define MONTHS
       #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
         "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
     (define DAYS
       #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
     (define tz-name (date*-time-zone-name (current-date)))
     (define d (date* (string->number second)
                      (string->number minute)
                      (string->number hour)
                      (string->number day)
                      (string->number month)
                      (string->number year)
                      0 0 #f 0 0
                      tz-name
                      ))
     (define weekday (~> d
                         date->seconds
                         seconds->date
                         date-week-day
                         DAYS))
     (str weekday ", "
          day " " (MONTHS (sub1 (string->number month))) " " year " "
          hour ":" minute ":" second " " tz-name)]
    [other
     (raise-argument-error
      'rfc-8601->822
      "date in ISO 8601 (YYYY-MM-DDThh:mm:ss) format" 0 s)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unlinkify-footnotes xs)
  (map unlinkify-footnotes/xexpr xs))

(define (unlinkify-footnotes/xexpr x)
  (match x
    ;; Footnote link to definition
    [`(sup (a ([href ,href][name ,name]) ,text)) `(sup ,text)]
    ;; Footnote definition return link
    [`(a ([href ,href]) "↩") ""]
    ;; All else
    [`(,(? symbol? tag) ([,(? symbol? ks) ,(? string? vs)] ...) . ,es)
     `(,tag ,(map list ks vs) ,@(unlinkify-footnotes es))]
    [`(,(? symbol? tag) . ,es)
     `(,tag ,@(unlinkify-footnotes es))]
    [_ x]))

(module+ test
  (check-equal?
   (unlinkify-footnotes/xexpr
    `(p "Blah blah" (sup (a ([href "x"][name "y"]) "1")) "."))
   `(p "Blah blah" (sup "1") "."))
  (check-equal?
   (unlinkify-footnotes/xexpr
    '(p "Blah" (em (sup (a ([href "x"][name "y"]) "1")) ".")))
   `(p "Blah" (em (sup "1") ".")))
  (check-equal?
   (unlinkify-footnotes/xexpr
    '(p (em "hi") "there"))
   '(p (em "hi") "there"))
  (check-equal?
   (unlinkify-footnotes/xexpr
    '(p ([class "foo"][style "x"]) "there"))
   '(p ((class "foo") (style "x")) "there"))
  (check-equal?
   (unlinkify-footnotes/xexpr
    '(p ([class "foo"]) "there"))
   '(p ((class "foo")) "there"))
  (check-equal?
   (unlinkify-footnotes/xexpr
    '(p () "there"))
   '(p () "there")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feed analytics without FeedBurner

;; If you want readership stats, but you can no longer use
;; FeedBurner.com (because Google shut down it and Google Reader):
;; Then another way to get feed stats is to decorate the links with
;; utm_xxx query parameters used by Google Analytics. (If Google shuts
;; down GA, too, naturally we'll come up with something else for that,
;; too.)
(define (full-uri/decorated uri-path #:source source #:medium medium)
  (str (full-uri uri-path)
       (cond [(current-decorate-feed-uris?)
              (str "?" "utm_source=" (our-encode source)
                   "&" "utm_medium=" (our-encode medium))]
             [else ""])))

;; full-uri/decorated handles the case of someone starting with the
;; feed and clicking through to the original web page. If you want to
;; count people reading it in a feed reader but _not_ clicking through
;; -- especially if you have the feed set to full posts not just
;; above-the-fold blurbs -- then we need to do an image bug. It is
;; also decorated with Google Analytics query params.
(define (feed-image-bug-xexpr uri-path #:source source #:medium medium)
  (cond [(current-feed-image-bugs?)
         `((img ([src ,(str (current-scheme/host)
                            "/img/1x1.gif"
                            "?" "utm_source=" (our-encode source)
                            "&" "utm_medium=" (our-encode medium)
                            "&" "utm_campaign=" (uri-encode uri-path))]
                 [height "1"]
                 [width "1"])))]
        [else '()]))

(module+ test
  (parameterize ([current-scheme/host "http://www.example.com"]
                 [current-feed-image-bugs? #t])
   (check-equal?
    (full-uri/decorated "/path/to/thing" #:source "all" #:medium "RSS")
    "http://www.example.com/path/to/thing?utm_source=all&utm_medium=RSS")
   (check-equal?
    (feed-image-bug-xexpr "/path/to/thing" #:source "all" #:medium "RSS")
    '((img ([src "http://www.example.com/img/1x1.gif?utm_source=all&utm_medium=RSS&utm_campaign=%2Fpath%2Fto%2Fthing"]
           [height "1"]
           [width "1"]))))))
