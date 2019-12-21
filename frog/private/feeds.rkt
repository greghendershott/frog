#lang racket/base

(require racket/require
         net/uri-codec
         (multi-in racket (date format match string))
         threading
         (only-in markdown xexpr->string)
         "author.rkt"
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         (only-in "util.rkt" display-to-file*)
         "verbosity.rkt")

(provide atom-feed-uri
         write-atom-feed
         rss-feed-uri
         write-rss-feed)

(module+ test
  (require rackunit))

;;; atom

(define (atom-feed-uri tag)
  (canonical-uri (~a "/feeds/" (slug tag) ".atom.xml")))

(define (write-atom-feed xs title tag of-uri-path file)
  (prn1 "Generating ~a" (abs->rel/www file))
  (define updated
    (match xs
      ['() "N/A"]
      [_   (rfc-8601/universal (post-date (car xs)))]))
  (~>
   `(feed
     ([xmlns "http://www.w3.org/2005/Atom"]
      [xml:lang "en"])
     (title ([type "text"]) ,(~a (current-title) ": " title))
     (link ([rel "self"]
            [href ,(full-uri (canonical-uri (abs->rel/www file)))]))
     (link ([href ,(full-uri of-uri-path)]))
     (id () ,(urn of-uri-path))
     ;; (etag () ???)
     (updated () ,updated)
     ,@(for/list ([x (in-list xs)]
                  [_ (in-range (current-max-feed-items))])
         (post->atom-feed-entry-xexpr tag x)))
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
    (id () ,(urn uri-path))
    (published () ,(rfc-8601/universal date))
    (updated () ,(rfc-8601/universal date))
    ;; When > 1 author, list them individually
    ,@(match (post-authors x)
        [(list) (list)]
        [(list _) (list)]
        [as (for/list ([a (in-list as)])
              `(author ,a))])
    ;; And to support readers that ignore all but the last one, have a final
    ;; element with the author(s) in one possibly comma-separated string.
    (author (name ,(post-author x)))
    (content
     ([type "html"])
     ,(cond [(current-feed-full?) body] ;but don't enhance-body
            [more? (string-append blurb
                                  (xexpr->string `(a ([href ,item-uri])
                                                   (em "More" hellip))))]
            [else blurb]))))

;;; rss

(define (rss-feed-uri tag)
  (canonical-uri (~a "/feeds/" (slug tag) ".rss.xml")))

(define (write-rss-feed xs title tag of-uri-path file)
  (prn1 "Generating ~a" (abs->rel/www file))
  (define updated
    (match xs
      ['() "N/A"]
      [_   (~> xs car post-date rfc-8601->rfc-822)]))
  (~>
   `(rss
     ([version "2.0"])
     (channel
      (title ,(~a (current-title) ": " title))
      (description ,(~a (current-title) ": " title))
      (link ,(full-uri of-uri-path))
      (lastBuildDate () ,updated)
      (pubDate ,updated)
      (ttl "1800")
      ,@(for/list ([x (in-list xs)]
                   [_ (in-range (current-max-feed-items))])
          (post->rss-feed-entry-xexpr tag x))))
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
    (guid ([isPermaLink "false"]) ,(urn uri-path))
    (pubDate () ,(~> date rfc-8601->rfc-822))
    (author ,(post-author x))
    (description
     ,(cond [(current-feed-full?) body] ;but don't enhance-body
            [more? (string-append blurb
                                  (xexpr->string `(a ([href ,item-uri])
                                                   (em "More" hellip))))]
            [else blurb]))))

;;; Datetime conversion

;; See rfc-8601->date for description of handling of time zones.
(define (rfc-8601/universal s)
  (~> s rfc-8601->date date->rfc-8601))

;; See rfc-8601->date for description of handling of time zones.
(define (rfc-8601->rfc-822 s)
  (~> s rfc-8601->date date->rfc-822))

;; Accepts 8601 time strings that either:
;;
;; (a) Explicitly have "Z", i.e. Universal Time.
;;
;; (b) Lack a "Z", in which case it's assumed to be local time and
;; converted to Universal Time.
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

(define (local->universal d)
  (~> d (date->seconds #t) (seconds->date #f)))

(define MONTHS
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define DAYS
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define (date->rfc-822 d)
  (match d
    [(date sc mn hr dy mo yr wd yd dst? tzo)
     (~a (vector-ref DAYS wd) ", "
         (2d dy) " " (vector-ref MONTHS (sub1 mo)) " " yr " "
         (2d hr) ":" (2d mn) ":" (2d sc) " UT")]))

(define (date->rfc-8601 d)
  (match d
    [(date sc mn hr dy mo yr wd yd dst? tzo)
     (~a yr "-" (2d mo) "-" (2d dy)
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

;;; footnotes

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

;;; urn

(define (urn uri-path)
  ;; Note that URNs have a more restricted syntax than URIs. Here we
  ;; just rely on `slug` to comply.
  (~a "urn:" (slug (current-scheme/host))
      ":" (slug uri-path)))

;;; Feed analytics without FeedBurner

;; If you want readership stats, but you can no longer use
;; FeedBurner.com (because Google shut down it and Google Reader):
;; Then another way to get feed stats is to decorate the links with
;; utm_xxx query parameters used by Google Analytics. (If Google shuts
;; down GA, too, naturally we'll come up with something else for that,
;; too.)
(define (full-uri/decorated uri-path #:source source #:medium medium)
  (~a (full-uri uri-path)
      (cond [(current-decorate-feed-uris?)
             (~a "?" "utm_source=" (uri-encode (slug source))
                 "&" "utm_medium=" (uri-encode (slug medium)))]
            [else ""])))
