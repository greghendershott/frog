#lang rackjure/base

(require net/uri-codec
         racket/date
         racket/function
         racket/list
         racket/match
         racket/string
         rackjure/str
         rackjure/threading
         (only-in markdown xexpr->string)
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
      [_   (rfc-8601/universal (post-date (first xs)))]))
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
    (published () ,(rfc-8601/universal date))
    (updated () ,(rfc-8601/universal date))
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
      [_   (~> xs first post-date rfc-8601->rfc-822)]))
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
    (pubDate () ,(~> date rfc-8601->rfc-822))
    (description
     "<html>"
     ,(cond [(current-feed-full?) body] ;don't enhance-body
            [more? (string-append blurb
                                  (xexpr->string `(a ([href ,item-uri])
                                                   (em "More" hellip))))]
            [else blurb])
     "</html>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datetimes

;; Ensures in UT
(define (rfc-8601/universal s)
  (~> s rfc-8601->date date->rfc-8601))

(define (rfc-8601->rfc-822 s)
  (~> s rfc-8601->date date->rfc-822))

(define (local->universal d)
  (~> d (date->seconds #t) (seconds->date #f)))

;; Only accepts "Z" time zone, or, no time zone at all in which latter
;; case the time is converted to Z (to universal time).
(define (rfc-8601->date s)
  (match s
    [(pregexp "(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(Z?)"
              (list _ year month day hour minute second Z?))
     (define zulu? (match Z? ["Z" #t] [_ #f]))
     (define d (date (string->number second)
                     (string->number minute)
                     (string->number hour)
                     (string->number day)
                     (string->number month)
                     (string->number year)
                      0 0 #f 0))
     (cond [zulu? d]
           [else (local->universal d)])]
    [other
     (raise-argument-error
      'rfc-8601->822
      "date in ISO 8601 (YYYY-MM-DDThh:mm:ss) format" 0 s)]))

(define MONTHS
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define DAYS
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define (date->rfc-822 d)
  (match d
    [(date sc mn hr dy mo yr wd yd dst? tzo)
     (str (DAYS wd) ", "
          (2d dy) " " (MONTHS (sub1 mo)) " " yr " "
          (2d hr) ":" (2d mn) ":" (2d sc) " UT")]))

(define (date->rfc-8601 d)
  (match d
    [(date sc mn hr dy mo yr wd yd dst? tzo)
     (str yr "-" (2d mo) "-" (2d dy)
          "T"
          (2d hr) ":" (2d mn) ":" (2d sc)
          "Z")]))

(define (2d n)
  (cond [(< n 10) (format "0~a" n)]
        [else     (format "~a" n)]))

(module+ test
  ;; Tests where timezone is already explicitly UT.
  (check-equal? (rfc-8601/universal "2014-06-01T00:00:00Z")
                "2014-06-01T00:00:00Z")
  (check-equal? (rfc-8601->rfc-822 "2014-06-01T00:00:00Z")
                "Sun, 01 Jun 2014 00:00:00 UT")
  (check-equal? (rfc-8601/universal "2014-10-11T00:00:00Z")
                "2014-10-11T00:00:00Z")
  (check-equal? (rfc-8601->rfc-822 "2014-10-11T00:00:00Z")
                "Sun, 11 Oct 2014 00:00:00 UT")

  ;; Tests where timezone is unspecified. Assume local time and
  ;; convert to UT. NOTE: These particular tests only work when run
  ;; from a machine where local timezone is ET.
  (match (date*-time-zone-name (current-date))
    [(or "EST" "EDT")
     ;; A date during EDT, 4 hour time difference from UT
     (define EDT-date "2014-06-01T00:00:00")
     (check-equal? (rfc-8601/universal EDT-date)
                   "2014-06-01T04:00:00Z")
     (check-equal? (rfc-8601->rfc-822 EDT-date)
                   "Sun, 01 Jun 2014 04:00:00 UT")
     ;; A date during EST, 5 hour time difference from UT
     (define EST-date "2014-10-11T00:00:00")
     (check-equal? (rfc-8601/universal EST-date)
                   "2014-10-11T04:00:00Z")
     (check-equal? (rfc-8601->rfc-822 EST-date)
                   "Sat, 11 Oct 2014 04:00:00 UT")]
    [_ (void)]))

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
