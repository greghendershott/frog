#lang rackjure  ;; dependency 1 of 2

(require markdown ;; dependency 2 of 2
         racket/runtime-path
         xml
         (prefix-in h: html)
         net/uri-codec
         racket/date
         (only-in srfi/1 break)
         (for-syntax racket/syntax)
         "config.rkt"
         "pygments.rkt"
         "take.rkt"
         "template.rkt"
         "watch-dir.rkt"
         "xexpr2text.rkt"
         "verbosity.rkt"
         ;; Remainder are just for the preview feature:
         web-server/servlet-env
         web-server/http
         web-server/dispatchers/dispatch)

(module+ test
  (require rackunit))

;; top is the project directory (e.g. the main dir in Git)
(define top (make-parameter #f))

;; For interactive development
(define-runtime-path example "../example/")

;; sources
(define (src-path) (build-path (top) "_src"))
(define (src/posts-path) (build-path (src-path) "posts"))

;; destinations, from root of the generated web site on down
(define (www-path) (build-path (top)))
(define (www/tags-path) (build-path (www-path) "tags"))
(define (www/feeds-path) (build-path (www-path) "feeds"))

;; Convert from absolute local path to what the URI path should be.
;; Ex: ~/project/css would become /css
(define (abs->rel/www path)
  (let ([path (path->string path)]
        [root (path->string (www-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) (str "/" x)]
      [_ (raise-user-error 'abs->rel/www "root: ~v path: ~v" root path)])))

;; Convert from absolute local path to one relative to project top dir.
;; Ex: ~/project/css would become css
;;
;; (Once upon a time (top) and (www-path) weren't necessarily the same.
;; Now they always are, and the only difference from abs->rel/www is the
;; lack of the leading slash. Could rewrite this.)
(define (abs->rel/top path)
  (let ([path (path->string path)]
        [root (path->string (top))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x]
      [_ (raise-user-error 'abs->rel/top "root: ~v path: ~v" root path)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (permalink-path year month day title filename pattern)
  (string? string? string? string? string? string? . -> . path?)
  (build-path (www-path)
              (regexp-replaces pattern
                               `([#rx"{year}" ,year]
                                 [#rx"{month}" ,month]
                                 [#rx"{day}" ,day]
                                 [#rx"{title}" ,title]
                                 [#rx"{filename}",filename]
                                 [#rx"^/" ""]))))

(module+ test
  (parameterize ([top (find-system-path 'temp-dir)])
    (define f (curry permalink-path
                     "2012" "05" "31" "title-of-post" "file-name"))
    (check-equal? (f "/{year}/{month}/{title}.html")
                  (build-path (top) "2012/05/title-of-post.html"))
    (check-equal? (f "/blog/{year}/{month}/{day}/{title}.html")
                  (build-path (top) "blog/2012/05/31/title-of-post.html"))
    (check-equal? (f "/blog/{year}/{month}/{day}/{title}/index.html")
                  (build-path (top) "blog/2012/05/31/title-of-post/index.html"))
    (check-equal? (f "/blog/{year}/{month}/{day}/{filename}/index.html")
                  (build-path (top) "blog/2012/05/31/file-name/index.html"))))

;; If the path-string ends in "/index.html", return the path without
;; the "index.html" suffix.
(define/contract (post-path->link pp)
  (path? . -> . string?)
  (~> (match (path->string pp)
        [(pregexp "^(.+?)/index.html" (list _ s)) (str s "/")]
        [s s])
      string->path
      abs->rel/www))

(module+ test
  (parameterize ([top (find-system-path 'temp-dir)])
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post.html"))
     "/blog/2012/05/31/title-of-post.html")
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post/index.html"))
     "/blog/2012/05/31/title-of-post/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters loaded from configuration file

(define current-scheme/host (make-parameter #f))
(define current-title (make-parameter #f))
(define current-author (make-parameter #f))
(define current-permalink (make-parameter #f))
(define current-index-full? (make-parameter #f)) ;index pages: full posts?
(define current-feed-full? (make-parameter #f))  ;feeds: full posts?
(define current-show-tag-counts? (make-parameter #t))
(define current-max-index-items (make-parameter 999))
(define current-max-feed-items (make-parameter 999))
(define current-decorate-feed-uris? (make-parameter #t))
(define current-feed-image-bugs? (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-tags (make-hash)) ;; (hashof string? exact-positive-integer?)

(define post-file-px #px"^(\\d{4}-\\d{2}-\\d{2})-(.+?)\\.(?:md|markdown)$")

(struct post (title      ;string?
              dest-path  ;path? - full pathname of local HTML file
              uri-path   ;string? - path portion of URI, with leading /
              date       ;string? - 8601 datetime format
              tags       ;(listof string?)
              blurb      ;(listof xexpr?) - the post summary
              more?      ;boolean? - is `body` more than just `blurb`?
              body       ;(listof xexpr?) - the full post xexprs, with
                         ;any <!-- more --> line removed, but NOT
                         ;syntax highlighted
              ))

;; Given a uri-path, prepend the scheme & host to make a full URI.
(define (full-uri uri-path)
  (str (current-scheme/host) uri-path))

;; A function for `fold-files` to gather post source files.
(define (read-post path type v)
  (cond
    [(eq? type 'file)
     (define-values (base name must-be-dir?) (split-path path))
     (match (path->string name)
       [(pregexp post-file-px (list _ dt nm))
        ;; Footnote prefix is date & name w/o ext e.g. "2010-01-02-a-name"
        (define footnote-prefix (~> (str dt "-" nm) string->symbol))
        (define xs (with-input-from-file path
                     (lambda () (read-markdown footnote-prefix))))
        ;; Split to the meta-data and the body
        (define-values (title date tags body) (meta-data xs))
        (cond [(member "DRAFT" tags)
               (prn0 (str "Skipping ~a\n"
                          "         because it has the tag, 'DRAFT'")
                     (abs->rel/top path))
               v]
              [else
               (prn1 "Reading ~a" (abs->rel/top path))
               ;; Add these tags to the set
               (for ([x tags])
                 (unless (equal? x "")
                   (hash-set! all-tags x (add1 (hash-ref all-tags x 0)))))
               ;; Split out the blurb (may be less than the entire body)
               (define-values (blurb more?) (above-the-fold body))
               ;; Make the destination HTML pathname
               (define year (substring date 0 4))
               (define month (substring date 5 7))
               (define day (substring date 8 10))
               (define dest-path
                 (permalink-path year month day
                                 (~> title string-downcase our-encode)
                                 (match (path->string name)
                                   [(pregexp post-file-px (list _ _ s)) s])
                                 (current-permalink)))
               ;; And return our result
               (cons (post title
                           dest-path
                           (post-path->link dest-path)
                           date
                           tags
                           blurb
                           more?
                           (filter (negate more-xexpr?) body))
                     v)])]
       [_ (prn2 (str "Skipping ~a\n"
                     "         Not named ~a")
                (abs->rel/top path)
                post-file-px)
          v])]
    [else v]))

(define (meta-data xs)
  (define px "^Title:\\s*(.+?)\nDate:\\s*(.+?)\nTags:\\s*(.*?)\n*$")
  (match xs
    [`((pre ,(pregexp px (list _ title date tags)))
       ,more ...)
     (values title date (tag-string->tags tags) more)]
    [`((pre () ,(pregexp px (list _ title date tags)))
       ,more ...)
     (values title date (tag-string->tags tags) more)]
    [`((pre () (code () ,(pregexp px (list _ title date tags))))
       ,more ...)
     (values title date (tag-string->tags tags) more)]
    [_ (raise-user-error 'meta-data "Post needs meta-data")]))

(module+ test
  (define s "Title: title\nDate: date\nTags: DRAFT\n\n")
  (check-not-exn (thunk (meta-data `((pre ,s)))))
  (check-not-exn (thunk (meta-data `((pre () ,s)))))
  (check-not-exn (thunk (meta-data `((pre () (code () ,s)))))))

(define (tag-string->tags s)
  (regexp-split #px",\\s*" s))

(define (above-the-fold xs)
  (define-values (above below) (break more-xexpr? xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(p ,(pregexp "\\s*<!-- more -->\\s*")) #t]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-post-page p older newer)
  (match-define (post title dest-path uri-path date tags blurb more? body) p)
  (prn1 "Generating post ~a" (abs->rel/top dest-path))
  (~> (render-template
       (src-path)
       "post-template.html"
       {'title title
        'uri-path uri-path
        'full-uri (full-uri uri-path)
        'date+tags (xexpr->string (date+tags->xexpr date tags))
        'content (xexprs->string (syntax-highlight body))
        'older-uri (and older (post-uri-path older))
        'newer-uri (and newer (post-uri-path newer))
        'older-title (and older (post-title older))
        'newer-title (and newer (post-title newer))})
      ;; bodies->page wants (listof xexpr?) so convert from string? to that
      string->xexpr/minimal-whitespace
      list
      (bodies->page #:title title
                    #:description (xexprs->description blurb)
                    #:uri-path uri-path
                    #:keywords tags)
      (display-to-file* dest-path #:exists 'replace)))

(define (date+tags->xexpr date tags)
  (define dt (substring date 0 10)) ;; just YYYY-MM-DD
  `(p ([class "date-and-tags"])
      (time ([datetime ,dt]
             [pubdate "true"]) ,dt)
      " :: "
      ,@(add-between (map tag->xexpr tags)
                     ", ")))

(define (tag->xexpr s)
  `(a ([href ,(str "/tags/" (our-encode s) ".html")]) ,s))

(define (string->xexpr/minimal-whitespace s)
  (parameterize ([collapse-whitespace #t])
    (string->xexpr s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bodies->page
;;
;; Put the body elements in a master page template.

(define (bodies->page contents                   ;(listof xexpr?)
                      #:title title              ;string?
                      #:description description  ;string?
                      #:uri-path uri-path        ;string?
                      #:feed [feed "all"]        ;string?
                      #:keywords [keywords '()]  ;listof string?
                      #:tag [tag #f]             ;(or/c string? #f)
                      #:toc-sidebar? [toc? #t])
  (render-template
   (src-path)
   "page-template.html"
   {'contents (xexprs->string contents)
    'title title
    'description description
    'uri-path uri-path
    'full-uri (full-uri uri-path)
    'atom-feed-uri (atom-feed-uri feed)
    'rss-feed-uri (rss-feed-uri feed)
    'keywords (string-join keywords ", ")
    'table-of-contents (cond [toc? (xexpr->string/pretty (toc-xexpr contents))]
                             [else ""])
    'tag tag
    'tags/feeds (xexprs->string (tags/feeds))}))

(define (xexprs->string xs)
  (string-join (map xexpr->string/pretty xs) "\n"))

(define (toc-xexpr xs)
  (match (toc xs)
    [`(div ([class "toc"]) (ol ,contents ...))
     (cond [(empty? contents) ""]
           [else `(div (p "On this page:"
                          (ol ([class "nav nav-list bs-docs-sidenav"])
                              ,@contents)))])]))

(define (tags/feeds)
  ;; Sort alphabetically by tag name. Use association list (can't sort
  ;; a hash).
  (define alist (~> (for/list ([(k v) (in-hash all-tags)])
                      (cons k v))
                    (sort string-ci<=? #:key car)))
  `((p "Tags:"
       (ul ,@(for/list ([(k v) (in-dict alist)])
               `(li ,(tag->xexpr k)
                    nbsp
                    ,@(if (current-show-tag-counts?) `(,(format "(~a)" v)) '())
                    " "
                    (a ([href ,(atom-feed-uri k)])
                       (img ([src "/img/feed.png"]))))))
    (p (a ([href "/index.html"]) "All Posts")
       " "
       (a ([href ,(atom-feed-uri "all")])
          (img ([src "/img/feed.png"])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-index xs    ;(listof post?) -> any
                     title ;string?
                     tag   ;(or/c #f string?)
                     feed  ;string?
                     file) ;path?
  (prn1 "Generating ~a" (abs->rel/top file))
  (~> (for/list ([x (in-list xs)]
                 [n (in-naturals)])
        (match-define
         (post title dest-path uri-path date tags blurb more? body) x)
        `(article
          ([class "index-post"])
          (header (h2 (a ([href ,uri-path]) ,title))
                  ,(date+tags->xexpr date tags))
          ,@(cond
             [(< n (current-max-index-items))
              `((div ([class "entry-content"])
                     ,@(cond [(current-index-full?) (syntax-highlight body)]
                             [more? `(,@(syntax-highlight blurb)
                                      (a ([href ,uri-path])
                                         (em "Continue reading ...")))]
                             [else (syntax-highlight blurb)])))]
             [else '()])))
      (bodies->page #:title title
                    #:description title
                    #:feed feed
                    #:uri-path (abs->rel/www file)
                    #:keywords (cond [tag (list tag)]
                                     [else (hash-keys all-tags)])
                    #:tag tag
                    #:toc-sidebar? tag) ;; no toc on home page
      (display-to-file* file #:exists 'replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (our-encode s)
  ;; Extremely conservative.
  ;;
  ;; WARNING: Changing this will break blog post permalink pattens that
  ;; use the {title} variable. Even if this could be improved, doing so
  ;; would break backward compatability.
  (~> (list->string (for/list ([c (in-string s)])
                      (cond [(or (char-alphabetic? c)
                                 (char-numeric? c)) c]
                            [else #\-])))
      (re* #px"-{2,}" "-")              ;only one hyphen in a row
      (re #px"-{1,}$" "")))             ;no hyphen at end

(define (re* s rx new)
  (regexp-replace* rx s new))
(define (re s rx new)
  (regexp-replace rx s new))

(module+ test
  (check-equal? (our-encode "Foo? Bar. Baz.")
                "Foo-Bar-Baz")
  (check-equal? (our-encode "Here's a question--how many hyphens???")
                "Here-s-a-question-how-many-hyphens"))

;; Less typing, but also returns its value so good for sticking in ~>
;; for debugging
(define (pp v)
  (pretty-print v)
  v)

;; Like display-to-file, but makes directories if needed.
(define (display-to-file* v path #:exists exists #:mode [mode 'binary])
  (make-directories-if-needed path)
  (display-to-file v path #:exists exists #:mode mode))

(define (make-directories-if-needed path)
  (with-handlers ([exn:fail? (const (void))])
    (define-values (base name dir?)(split-path path))
    (make-directory* base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-highlight xs)
  (append* (for/list ([x xs])
             (match x
               [`(pre ([class ,brush]) ,text)
                (match brush
                  [(pregexp "\\s*brush:\\s*(.+?)\\s*$" (list _ lang))
                   (pygmentize text lang)]
                  [_ `((pre ,text))])]
               [_ (list x)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom-feed-uri tag)
  (str "/feeds/" tag ".atom.xml"))

(define (write-atom-feed xs title tag of-uri-path file)
  (prn1 "Generating ~a" (abs->rel/top file))
  (define updated (str (post-date (first xs)) "Z")) ;; lie: not nec. UTC
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
   xexpr->string/pretty
   (list "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
   reverse
   string-join
   string->bytes/utf-8
   (display-to-file* file #:exists 'replace)))

(define (post->atom-feed-entry-xexpr tag x)
  (match-define (post title dest-path uri-path date tags blurb more? body) x)
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
     ,(xexpr->string/pretty
       `(html
         ,@(feed-image-bug-xexpr uri-path #:source tag #:medium "Atom")
         ,@(~> (cond [(current-feed-full?) body] ;don't syntax-highlight
                     [more? `(,@blurb
                              (a ([href ,item-uri])
                                 (em "Continue reading ...")))]
                     [else blurb])
               unlinkify-footnotes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rss-feed-uri tag)
  (str "/feeds/" tag ".rss.xml"))

(define (write-rss-feed xs title tag of-uri-path file)
  (prn1 "Generating ~a" (abs->rel/top file))
  (define updated (~> xs first post-date rfc-8601->822))
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
   xexpr->string/pretty
   (list "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
   reverse
   string-join
   string->bytes/utf-8
   (display-to-file* file #:exists 'replace)))

(define (post->rss-feed-entry-xexpr tag x)
  (match-define (post title dest-path uri-path date tags blurb more? body) x)
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
     ,(xexpr->string/pretty
       `(html
         ,@(feed-image-bug-xexpr uri-path #:source tag  #:medium "RSS")
         ,@(~> (cond [(current-feed-full?) body] ;don't syntax-highlight
                     [more? `(,@blurb
                              (a ([href ,item-uri])
                                 (em "Continue reading ...")))]
                     [else blurb])
               unlinkify-footnotes))))))

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
          hour ":" minute ":" second " " tz-name)]))

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
    [`(,(? symbol? tag) ([,(? symbol? ks) ,(? string? vs)] ...) ,es ...)
     `(,tag ,(map list ks vs) ,@(unlinkify-footnotes es))]
    [`(,(? symbol? tag) ,es ...)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define new-post-template
#<<EOF
    Title: ~a
    Date: ~a
    Tags: DRAFT

_Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published._

EOF
)

(define (new-post title)
  (parameterize ([date-display-format 'iso-8601])
    (define d (current-date))
    (define filename (str (~> (str (date->string d #f) ;omit time
                                   "-"
                                   (~> title string-downcase))
                              our-encode)
                          ".md"))
    (define pathname (build-path (src/posts-path) filename))
    (when (file-exists? pathname)
      (raise-user-error 'new-post "~a already exists." pathname))
    (display-to-file* (format new-post-template
                              title
                              (date->string d #t)) ;do includde time
                      pathname
                      #:exists 'error)
    (displayln pathname)
    ;; (define editor (getenv "EDITOR"))
    ;; (when editor
    ;;   (system (format "~a ~a &" editor (path->string pathname))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: This doesn't delete generic HTML files generated from
;; Markdown files, it only deletes those genereated from post files
;; (of a certain name format).
(define (clean)
  (define (maybe-delete path type v)
    (define (rm p)
      (delete-file p)
      (prn0 "Deleted ~a" (abs->rel/top p)))
    (cond
     [(eq? type 'file)
      (define-values (base name must-be-dir?) (split-path path))
      (cond [(equal? path (build-path (www-path) "index.html"))
             (rm path)]
            [(equal? (build-path base) (build-path (www-path) "tags/"))
             (rm path)]
            [(equal? (build-path base) (build-path (www-path) "feeds/"))
             (rm path)]
            [else (match (path->string path)
                    [(pregexp "\\d{4}/\\d{2}/.+?\\.html$")
                     (rm path)]
                    [_ (void)])])]))
  (fold-files maybe-delete '() (www-path) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xexpr->string/pretty x)
  (with-output-to-string (thunk (display-xexpr x))))

(define (build-non-post-pages) ;; -> (listof string?)
  (fold-files write-non-post-page '() (src-path) #f))

(define (write-non-post-page path type v)
  (define-values (base name must-be-dir?) (split-path path))
  (cond
   [(and (eq? type 'file)
         (regexp-match? #px"\\.(?:md|markdown)$" path)
         (not (regexp-match? post-file-px (path->string name))))
    (prn1 "Reading non-post ~a" (abs->rel/top path))
    (define xs (syntax-highlight (with-input-from-file path read-markdown)))
    (define dest-path
      (build-path (www-path)
                  (apply build-path
                         (~> path
                             (path-replace-suffix ".html")
                             abs->rel/www
                             explode-path
                             cddr)))) ;lop off the "/" and "_src" parts
    (define title
      (match xs
        ;; First h1 header, if any
        [(list-no-order `(h1 (,_ ...) ... ,els ...) _ ...)
         (string-join (map xexpr->markdown els) "")]
        ;; Else name of the source file
        [_ (~> path
               (path-replace-suffix "")
               file-name-from-path
               path->string)]))
    (define uri-path (abs->rel/www dest-path))
    (prn1 "Generating non-post ~a" (abs->rel/top dest-path))
    (~> xs
        (bodies->page #:title title
                      #:description (xexprs->description xs)
                      #:uri-path uri-path)
        (display-to-file* dest-path #:exists 'replace))
    (cons uri-path v)]
   [else v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build)
  (start-pygments)
  ;; Read the posts and get (listof post?) with which to do more work.
  ;; Sort the list by date, newest first.
  (define (post<=? a b)
    (string<=? (post-date a) (post-date b)))
  (set! all-tags (make-hash))
  (define posts (~> (fold-files read-post '() (src/posts-path) #f)
                    (sort (negate post<=?))))
  ;; Make lists for older and newer items. Just shift each direction
  ;; and add #f to one end.
  (define older (append (drop posts 1) (list #f)))
  (define newer (cons #f (take posts (sub1 (length posts)))))
  ;; Write the post pages
  (for-each write-post-page posts older newer)
  ;; For each tag, write an index page, Atom feed, RSS feed
  (define (post-has-tag? tag post)
    (member tag (post-tags post)))
  (for ([(tag _) (in-hash all-tags)])
    (define posts-this-tag (filter (curry post-has-tag? tag) posts))
    (define tag-index-path
      (build-path (www/tags-path) (str (our-encode tag) ".html")))
    (write-index posts-this-tag
                 (str "Posts tagged '" tag "'")
                 tag
                 (our-encode tag)
                 tag-index-path)
    (write-atom-feed posts-this-tag
                     (str "Posts tagged '" tag "'")
                     tag
                     (abs->rel/www tag-index-path)
                     (build-path (www/feeds-path) (str (our-encode tag)
                                                       ".atom.xml")))
    (write-rss-feed posts-this-tag
                    (str "Posts tagged '" tag "'")
                    tag
                    (abs->rel/www tag-index-path)
                    (build-path (www/feeds-path) (str (our-encode tag)
                                                      ".rss.xml"))))
  ;; Write the index page for all posts
  (write-index posts (current-title) #f "all"
               (build-path (www-path) "index.html"))
  ;; Write Atom feed for all posts
  (write-atom-feed posts "All Posts" "all" "/index.html"
                   (build-path (www/feeds-path) "all.atom.xml"))
  ;; Write RSS feed for all posts
  (write-rss-feed posts "All Posts" "all" "/index.html"
                   (build-path (www/feeds-path) "all.rss.xml"))
  ;; Generate non-post pages.
  (define pages (build-non-post-pages))
  ;; Write sitemap
  ;; One gotcha: What about other "sub-sites", for example GitHub project
  ;; pages?  How to include them in sitemap.txt?
  (prn1 "Generating sitemap.txt")
  (with-output-to-file (build-path (www-path) "sitemap.txt")
    #:exists 'replace
    (thunk (for ([x (in-list (map full-uri
                                  (append (map post-uri-path posts) pages)))])
             (displayln x))))
  (stop-pygments)
  (let* ([d (current-date)]
         [n (lambda (x) (~r (x d) #:min-width 2 #:pad-string "0"))])
    (prn0 (~a (n date-hour) ":" (n date-minute) " Done generating files")))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For use with `watch`. For quicker edit/view cycle. Try to build one
;; markdown file and only its HTML output (at the cost of leaving all
;; the other dependencies out of date, and with bogus older/newer
;; links for a post page.)  If can do this, return #t.  Else if can't
;; (b/c `path` isn't a post markdown or non-post page markdown) return
;; #f in which case caller might want to do a full build.
(define (build-one path)
  (define-values (base name dir?) (split-path path))
  (cond [(equal? (str base) ;a post md file?
                 (str (src/posts-path) "/"))
         (start-pygments)
         (match (read-post path 'file '())
           [(list p) (write-post-page p p p) #t]
           [_ #f])
         (stop-pygments)]
        [else (match name
                [(pregexp "\\.(md|markdown)$")
                 (start-pygments)
                 (build-non-post-pages)
                 (stop-pygments)]
                [_ #f])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (preview [port 3000])
  ;; `serve/servlet` uses the `send-url` from `net/sendurl`, which
  ;; (unlike the `send-url` from `external/browser`) doesn't prompt
  ;; the user if no external browser preference is set. This can
  ;; matter on some Linux distributions, e.g. Ubuntu which needs
  ;; xdg-open or sensible-browser, but `net/snedurl` uses neither and
  ;; doesn't even prompt the user. So check for this case here, and if
  ;; no browser preference set yet, ask the user, like the `send-url`
  ;; from `external/browser` would do.
  (when (eq? 'unix (system-type 'os))
    (unless (get-preference 'external-browser)
      (define ask (dynamic-require 'browser/external 'update-browser-preference))
      (ask #f)))
  (serve/servlet (lambda (_) (next-dispatcher))
                 #:servlet-path "/"
                 #:extra-files-paths (list (www-path))
                 #:port port
                 #:launch-browser? #t
                 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (watch)
  (build)
  (define t
    (watch-directory (build-path (top))
                     '(file)
                     (lambda (path what)
                       (match (path->string path)
                         ;; Output file
                         [(pregexp "\\.(?:html|xml|txt)") (void)]
                         ;; Source file
                         [_ (unless (build-one path)
                              (build))
                            (displayln #"\007")])) ;beep (hopefully)
                     #:rate 5))
  (preview)
  (kill-thread t)
  (build))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (init-project)
  (define (copy path)
    (define from (~> (build-path example path) simplify-path))
    (define to   (~> (build-path (top) path) simplify-path))
    (prn0 "~a" to)
    (make-directories-if-needed to)
    (copy-directory/files from to))
  (prn0 "Creating files in ~a:" (build-path (top)))
  (copy ".frogrc")
  (copy "_src/About.md")
  (copy "_src/page-template.html")
  (copy "_src/post-template.html")
  (copy "_src/posts/2012-01-01-a-2012-blog-post.md")
  (copy "css/")
  (copy "js/")
  (copy "img/")
  (prn0 "Project ready. Try `raco frog -bp` to build and preview."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For interactive development
(define (build/preview)
  (parameterize* ([top example]
                  [current-verbosity 99])
    (parameterize-from-config (build-path (top) ".frogrc")
                              ([scheme/host "http://www.example.com"]
                               [title "Untitled Site"]
                               [author "The Unknown Author"]
                               [show-tag-counts? #t]
                               [permalink "/{year}/{month}/{title}.html"]
                               [index-full? #f]
                               [feed-full? #f]
                               [max-index-items 999]
                               [max-feed-items 999]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f])
      ;; (clean)
      (build)
      (preview)
      ;; (watch)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (require "../info.rkt")
  (printf "Frog ~a\n" (#%info-lookup 'version))
  (parameterize* ([top (current-directory)])
    (parameterize-from-config (build-path (top) ".frogrc")
                              ([scheme/host "http://www.example.com"]
                               [title "Untitled Site"]
                               [author "The Unknown Author"]
                               [show-tag-counts? #t]
                               [permalink "/{year}/{month}/{title}.html"]
                               [index-full? #f]
                               [feed-full? #f]
                               [max-index-items 999]
                               [max-feed-items 999]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f])
      (command-line
       #:program "frog"
       #:once-each
       [("--init")
        (""
         "Initialize current directory as a new Frog project, creating"
         "default files as a starting point.")
        (init-project)]
       [("-n" "--new") title
        (""
         "Create a .md file for a new post based on today's date and <title>.")
        (new-post title)]
       [("-m" "--make" "-b" "--build")
        (""
         "Generate files.")
        (build)]
       [("-p" "--preview")
        (""
         "Run a local web server and start your browser on blog home page.")
        (preview)]
       [("-c" "--clean")
        (""
         "Delete generated files.")
        (clean)]
       [("-w" "--watch")
        (""
         "(Experimental: Only rebuilds some files.)"
         "1. Generate files."
         "2. Run a local web server and start your browser."
         "3. Watch for changed files, and generate again."
         "   (You'll need to refresh the browser yourself.")
        (watch)]
       #:once-any
       [("-v" "--verbose") "Verbose. Put first."
        (current-verbosity 1)
        (prn1 "Verbose mode")]
       [("-V" "--very-verbose") "Very verbose. Put first."
        (current-verbosity 2)
        (prn2 "Very verbose mode")]))))
