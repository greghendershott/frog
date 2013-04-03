#lang rackjure  ;; extra dependency 1 of 2

(require markdown ;; extra dependency 2 of 2
         racket/runtime-path
         xml
         (prefix-in h: html)
         net/uri-codec
         racket/date
         (only-in srfi/1 break)
         (for-syntax racket/syntax)
         ;; Remainder are just for the preview feature:
         web-server/servlet-env
         web-server/http
         web-server/dispatchers/dispatch)

;; top is the project directory (e.g. the main dir in Git)
(define top (make-parameter #f))

;; For interactive development
(define-runtime-path example "example/")

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
      [else (raise-user-error 'abs->rel/top "root: ~v path: ~v" root path)])))

;; Convert from absolute local path to one relative to project top dir.
;; Ex: ~/project/css would become css
;; (Once upon a time (top) and (www-path) weren't necessarily the same.
;; Now they always are, and the only difference from abs->rel/www is the
;; lack of the leading slash. Could rewrite this.)
(define (abs->rel/top path)
  (let ([path (path->string path)]
        [root (path->string (top))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x]
      [else (raise-user-error 'abs->rel/top "root: ~v path: ~v" root path)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters loaded from configuration file

(define current-verbosity (make-parameter 0))
(define current-scheme/host (make-parameter #f))
(define current-title (make-parameter #f))
(define current-author (make-parameter #f))
(define current-index-full? (make-parameter #f)) ;index pages: full posts?
(define current-feed-full? (make-parameter #f))  ;feeds: full posts?
(define current-max-index-items (make-parameter 999))
(define current-max-feed-items (make-parameter 999))
(define current-google-analytics-account (make-parameter #f))
(define current-google-analytics-domain (make-parameter #f))
(define current-disqus-shortname (make-parameter #f))
(define current-pygments-pathname (make-parameter #f))
(define current-decorate-feed-uris? (make-parameter #t))
(define current-feed-image-bugs? (make-parameter #f))
(define current-older/newer-buttons (make-parameter "both"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; verbosity

(define (prn level fmt . vs)
  (when (>= (current-verbosity) level)
    (apply printf fmt vs)
    (newline)))

(define-syntax (define-prn stx)
  (syntax-case stx ()
    [(_ level)
     (with-syntax ([id (format-id stx "prn~a" (syntax-e #'level))])
       #'(define (id fmt . vs)
           (apply prn level fmt vs)))]))

(define-prn 0)
(define-prn 1)
(define-prn 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-tags (make-hash)) ;; (hashof string? exact-positive-integer?)

(define post-file-px #px"^\\d{4}-\\d{2}-\\d{2}-(.+?)\\.(?:md|markdown)$")

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
       [(pregexp post-file-px)
        (define xs (with-input-from-file path read-markdown))
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
               (define dest-path
                 (build-path (www-path) year month
                             (str (~> title string-downcase our-encode)
                                  ".html")))
               ;; And return our result
               (cons (post title
                           dest-path
                           (abs->rel/www dest-path)
                           date
                           tags
                           blurb
                           more?
                           (filter (negate more-xexpr?) body))
                     v)])]
       [else
        (prn2 (str "Skipping ~a\n"
                   "         Not named ~a")
              (abs->rel/top path)
              post-file-px)
        v])]
    [else v]))

(define (maybe-make-directory p)
  (unless (directory-exists? p)
    (prn0 "Creating directory ~a" (abs->rel/top p))
    (make-directory p)))

(define (write-post-page p older newer)
  (match-define (post title dest-path uri-path date tags blurb more? body) p)
  (prn1 "Generating post ~a" (abs->rel/top dest-path))
  (~> (post-xexpr title uri-path date tags body older newer)
      (bodies->page #:title title
                    #:description (xexprs->description blurb)
                    #:uri-path uri-path
                    #:keywords tags)
      xexpr->string/pretty
      (list "<!DOCTYPE html>")
      reverse
      string-join
      string->bytes/utf-8
      (display-to-file* dest-path #:exists 'replace)))

(define (post-xexpr title uri-path date tags body older newer)
  `((h1 ,title)
    ,(date+tags->xexpr date tags)
    ,@(syntax-highlight body)
    (p 'nbsp)
    ,(social uri-path)
    (p 'nbsp)
    ,(older/newer-nav older newer)))

(define (older/newer-nav older newer)
  `(ul
    ([class "pager"])
    ,(cond [newer
            `(li ([class "previous"])
                 (a ([href ,(post-uri-path newer)])
                    ,@(match (current-older/newer-buttons)
                        ["both"  `(larr "Newer" nbsp (em ,(post-title newer)))]
                        ["age"   `(larr "Newer")]
                        ["title" `(larr (em ,(post-title newer)))])))]
           [else
            `(li ([class "previous disabled"])
                 (a ([href "#"])
                    'larr "Newer"))])
    ,(cond [older
            `(li ([class "next"])
                 (a ([href ,(post-uri-path older)])
                    ,@(match (current-older/newer-buttons)
                        ["both"  `((em ,(post-title older)) nbsp "Older" rarr)]
                        ["age"   `("Older" rarr)]
                        ["title" `((em ,(post-title older)) rarr)])))]
           [else
            `(li ([class "next disabled"])
                 (a ([href "#"])
                    "Older" 'rarr))])))

(define (meta-data xs)
  (match (first xs)
    [`(pre ,s)
     (match s
       [(pregexp "^Title: (.+?)\nDate: (.+?)\nTags:\\s*(.*?)\n*$"
                 (list _ title date tags))
        (values title date (tag-string->tags tags) (rest xs))]
       [else (raise-user-error 'meta-data "Missing meta-data")])]))

(define (tag->xexpr s)
  `(a ([href ,(str "/tags/" (our-encode s) ".html")]) ,s))

(define (date+tags->xexpr date tags)
  `(p ,(substring date 0 10) ;; just YYYY-MM-DD
      " :: "
      ,@(add-between (map tag->xexpr tags)
                     ", ")))

(define (tag-string->tags s)
  (regexp-split #px",\\s*" s))

(define (above-the-fold xs)
  (define-values (above below) (break more-xexpr? xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [(list p "<!-- more -->") #t]
    [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers to reduce redundancy in xexprs

(define (meta k v)
  `(meta ([name ,k]
          [content ,v])))

(define (link/css href)
  `(link ([href ,href]
          [rel "stylesheet"]
          [type "text/css"])))

;; Link to JS
(define (script/js src)
  `(script ([src ,src]
            [type "text/javascript"])))

;; Include local JS, with format -- can be "templated" JS with
;; formatters like ~a. Use case: Plug in some account number or name.
(define (script/js/inc v . vs)
  (let* ([s (cond [(path? v) (file->string v)]
                  [(string? v) v]
                  [else (raise-type-error 'script/js/inc "path? or ?str" v)])]
         [js (apply format s vs)])
    `(script ([type "text/javascript"])
             ,js)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bodies->page
;;
;; Put the body elements in a master page template.

;; Some static JS we "include"
(define-runtime-path google-analytics.js "google-analytics.js")
(define-runtime-path tweet-button.js "tweet-button.js")
(define-runtime-path disqus.js "disqus.js")

;; Bootstrap has some permutations: Responsive or not, minified or not:
(define responsive? (make-parameter #f)) ;; Responsive not working ?!?!
(define minified? (make-parameter #t))

(define (bootstrap-js)
  (script/js (cond [(minified?) "/js/bootstrap.min.js"]
                   [else "/js/bootstrap.js"])))

(define (bootstrap-css)
  (link/css (cond [(minified?)
                   (cond [(responsive?) "/css/bootstrap-responsive.min.css"]
                         [else "/css/bootstrap.min.css"])]
                  [else
                   (cond [(responsive?) "/css/bootstrap-responsive.css"]
                         [else "/css/bootstrap.css"])])))

(define (bs-container)
  (if (responsive?) "container-fluid" "container"))

(define (bs-row)
  (if (responsive?) "row-fluid" "row"))

;; And now our Feature Presentation:
(define (bodies->page xs                         ;listof xexpr?
                      #:title title              ;string?
                      #:description description  ;string?
                      #:uri-path uri-path        ;string?
                      #:feed [feed "all"]        ;string?
                      #:keywords [keywords '()]) ;listof string?
  ;; -> xexpr?
  `(html ([lang "en"])
         (head (meta ([charset "utf-8"]))
               (title ,title)
               ,(meta "description" description)
               ,(meta "author" (current-author))
               ,(meta "keywords" (string-join keywords ","))
               (link ([rel "canonical"][href ,(full-uri uri-path)]))
               (link ([href "/favicon.ico"][rel "shortcut icon"]))
               (meta ([name "viewport"]
                      [content "width=device-width, initial-scale=1.0"]))
               ;; CSS
               ,(bootstrap-css)
               ,(link/css "/css/pygments.css")
               ,(link/css "/css/custom.css")
               ;; Atom feed
               (link ([href ,(atom-feed-uri feed)]
                      [type "application/atom+xml"]
                      [rel "alternate"]
                      [title ,(str (current-title) ": " feed)]))
               ;; RSS feed
               (link ([href ,(rss-feed-uri feed)]
                      [type "application/rss+xml"]
                      [rel "alternate"]
                      [title ,(str (current-title) ": " feed)]))
               ;; JS
               ,(script/js "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js")
               ,(bootstrap-js)
               ,@(google-analytics))
         (body
          ,(navbar uri-path)
          (div ([class ,(bs-container)])
               (div ([class ,(bs-row)])
                    ;; Span2: Docs sidebar
                    (div ([id "left-sidebar"]
                          [class "span2 bs-docs-sidebar"])
                         ,(toc-xexpr xs)
                         (p nbsp))
                    ;; Span8: Main content
                    (div ([id "content"]
                          [class "span8"])
                         ;; Caller's content
                         ,@xs)
                    ;; Span2: Tags list
                    (div ([id "right-sidebar"]
                          [class "span2"])
                         ,(tags/feeds-xexpr)))
               (footer
                (hr)
                ,@(with-input-from-file
                      (build-path (src-path) "footer.md")
                    read-markdown)))
          )))

(define (navbar active-uri-path)
  `(div ([class "navbar navbar-inverse"])
        (div ([class "navbar-inner"])
             (div ([class "container"])
                  ,(nav-ul (read-navbar) active-uri-path)))))

(define read-navbar
  (let ([navbar-xexpr #f]) ;; read navbar.md on-demand, memoize
    (lambda ()
      (unless navbar-xexpr
        (define /md (build-path (src-path) "navbar.md"))
        (set! navbar-xexpr
              (cond [(file-exists? /md)
                     (define md (abs->rel/top /md))
                     (match (with-input-from-file /md read-markdown)
                       [`((ul ,xs ...))
                        (prn1 "Read bulleted list for navbar from ~a" md)
                        xs]
                       [else
                        (prn0 "Bulleted list not found; ignoring ~a" md)
                        '()])]
                    [else
                     (prn1 "~a not found; no extra navbar items" /md)
                     '()])))
      navbar-xexpr)))

(define (nav-ul items active-uri-path)
  `(ul ([class "nav"])
       (li ([style "width: 60px;"])
           (img ([style "width: 42px; height:33px;"]
                 [src "/img/navbar-logo.jpg"])))
       (li (a ([href "/index.html"][class "brand"]) ,(current-title)))
       ,(nav-li "/index.html" "Home" active-uri-path)
       ,@(for/list ([item items])
           (match item
             [`(li (a ([href ,uri]) ,text))
              (nav-li uri text active-uri-path)]
             [`(ul ,items ...) (nav-ul items active-uri-path)]
             [else item]))))

(define (nav-li href text uri-path)
  `(li ,(cond [(string-ci=? href uri-path) `([class "active"])]
              [else `()])
       (a ([href ,href]) ,text)))

(define (google-analytics)
  (cond [(and (current-google-analytics-account)
              (current-google-analytics-domain))
         `(,(script/js/inc google-analytics.js
                           (current-google-analytics-account)
                           (current-google-analytics-domain)))]
        [else '()]))

(define (social uri-path)
  `(p
    ;; Twitter
    ,(script/js/inc tweet-button.js)
    (a ([href "https://twitter.com/share"]
        [class "twitter-share-button"]
        [data-url ,(full-uri uri-path)]
        [data-dnt "true"])
       "Tweet")
    ;; Google+
    ,(script/js "https://apis.google.com/js/plusone.js")
    (g:plusone ([size "medium"]
                [href ,(full-uri uri-path)]))
    ;; Disqus for comments (but only if (current-disqus-shortname) not #f)
    ,@(cond [(current-disqus-shortname)
             `(,(script/js/inc disqus.js (current-disqus-shortname))
               (div ([id "disqus_thread"])))]
            [else `()])))

(define (toc-xexpr xs)
  (match (toc xs)
    [`(div ([class "toc"]) (ol ,contents ...))
     (cond [(empty? contents) ""]
           [else `(div (p "On this page:"
                          (ol ([class "nav nav-list bs-docs-sidenav"])
                              ,@contents)))])]))

(define (tags/feeds-xexpr)
  (define alist (~> (for/list ([(k v) (in-hash all-tags)])
                      (cons k v))
                    (sort string-ci<=? #:key car)))
  `(div
    (p "Tags:"
       (ul ,@(for/list ([(k v) (in-dict alist)])
               `(li ,(tag->xexpr k)
                    nbsp
                    ,(format "(~a)" v)
                    " "
                    (a ([href ,(atom-feed-uri k)])
                       (img ([src "/img/feed.png"]))))))
    (p (a ([href "/index.html"]) "All Posts")
       " "
       (a ([href ,(atom-feed-uri "all")])
          (img ([src "/img/feed.png"])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Like `take`, but OK if list has fewer than members
(define (take<= xs n)
  (for/list ([x (in-list xs)]
             [_ (in-range n)])
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-index xs    ;(listof post?) -> any
                     title ;string?
                     tag   ;or/c #f string?
                     feed  ;string?
                     file) ;path?
  (prn1 "Generating ~a" (abs->rel/top file))
  (~> (cons
       `(h1 ,@(cond [tag `("Posts tagged " (em ,tag))]
                    [else `(,title)]))
       (for/list ([x (in-list (take<= xs (current-max-index-items)))])
        (match-define
         (post title dest-path uri-path date tags blurb more? body) x)
        `(div ([class "index-post"])
              (h2 (a ([href ,uri-path]) ,title))
              ,(date+tags->xexpr date tags)
              ,@(cond [(current-index-full?) (syntax-highlight body)]
                      [more? `(,@(syntax-highlight blurb)
                               (a ([href ,uri-path])
                                  (em "Continue reading ...")))]
                      [else (syntax-highlight blurb)]))))
      (add-between `(hr))
      (bodies->page #:title title
                    #:description title
                    #:feed feed
                    #:uri-path (abs->rel/www file)
                    #:keywords (cond [tag (list tag)]
                                     [else (hash-keys all-tags)]))
      xexpr->string/pretty
      (list "<!DOCTYPE html>")
      reverse
      string-join
      string->bytes/utf-8
      (display-to-file* file #:exists 'replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (our-encode s)
  ;; Extremely conservative.
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
                  [else `((pre ,text))])]
               [else (list x)]))))

(define (pygmentize text lang)
  (cond [(current-pygments-pathname)
         (prn2 "  system call to pygmentize")
         (define tmp-in (make-temporary-file))
         (define tmp-out (make-temporary-file))
         (display-to-file text tmp-in #:exists 'replace)
         (define cmd (str #:sep " "
                          (expand-user-path (current-pygments-pathname))
                          "-f html"
                          "-O linenos=1"
                          "-l" lang
                          "<" tmp-in
                          "> "tmp-out))
         (define code (system/exit-code cmd))
         (cond [(zero? code)
                (define (elements->element xs)
                  (make-element #f #f '*root '() xs))
                (with-input-from-file tmp-out
                  (thunk
                   (parameterize ([permissive-xexprs #t])
                     (~> (h:read-html-as-xml)
                         elements->element
                         xml->xexpr
                         cddr))))]
               [else `((pre ,text))])]
        [else `((pre ,text))]))

;; (define xp
;;   (parameterize ([current-pygments-pathname
;;                   "~/src/python/pygments-main/pygmentize"])
;;     (first
;;      (pygmentize "<pre class=\"brush: racket\">'(#:a 10 #:b b #:c c)</pre>"
;;                  "racket"))))
;; (pretty-print xp)
;; (displayln (xexpr->string xp))
;; (display-xexpr xp) (newline)


(define (pygments.css)
  (path->string (build-path (www-path) "css" "pygments.css")))

(define (make-pygments.css style)
  (when (current-pygments-pathname)
    (define cmd (str #:sep " "
                     (expand-user-path (current-pygments-pathname))
                     "-f html"
                     "-S " style
                     "> " (pygments.css)))
    (prn0 "Generating css/pygments.css")
    (system cmd)
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xexprs->description xs [num 3])
  (str (apply str #:sep " " (for/list ([i (in-range num)]
                                       [x (in-list xs)])
                              (xexpr->markdown x)))
       " ..."))

(module+ test
  (require rackunit)
  (check-equal?
   (xexprs->description '((h1 ([class "foo"]) "A heading")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff."))
                        3)
   "A heading: A _paragraph_ of some stuff. A _paragraph_ of some stuff. ...")
  (check-equal?
   (xexprs->description '((h1 ([class "foo"]) "A heading")
                          (p "A " (em "paragraph") " of some stuff.")
                          (img ([src "blah"]))
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff."))
                        3)
   "A heading: A _paragraph_ of some stuff.  ..."))

;; Not full markdown, just a "lite" variation.
(define (xexpr->markdown x)
  (define (do x)
    (match x
      [`(em ,_ ... ,s) (str "_" s "_")]
      [`(strong ,_ ... ,s) (str "**" s "**")]
      [`(h1 ,_ ... ,s) (str s ":")]
      [`(h2 ,_ ... ,s) (str s ":")]
      [`(h3 ,_ ... ,s) (str s ":")]
      [`(,tag ([,_ ,_] ...) ... ,es ...) (str
                                          (for/fold ([s ""]) ([e (in-list es)])
                                            (str s  (do e)))
                                          (match tag
                                            [(or 'p 'li 'blockquote) " "]
                                            [else ""]))]
      [(? string? s) s]
      ['ndash "--"]
      ['mdash "--"]
      [(? symbol? s) (str "&" s ";")]
      [else ""])) ;; ignore others
  ;; Kill trailing space
  (match (do x) [(pregexp "^(.*?)\\s*$" (list _ x)) x]))

(module+ test
  (require rackunit)
  (check-equal? (xexpr->markdown '(em "foobar"))
                "_foobar_")
  (check-equal? (xexpr->markdown '(em ([class "foo"]) "foobar"))
                "_foobar_")
  (check-equal? (xexpr->markdown '(strong "foobar"))
                "**foobar**")
  (check-equal? (xexpr->markdown '(strong ([class "foo"]) "foobar"))
                "**foobar**")
  (check-equal? (xexpr->markdown '(p "I am some " (em "emphasized") " text"))
                "I am some _emphasized_ text")
  (check-equal? (xexpr->markdown '(p ([class "foo"])
                                     "I am some " (em "emphasized") " text"))
                "I am some _emphasized_ text")
  (check-equal? (xexpr->markdown '(p "M" 'amp "Ms" 'mdash "gotta love 'em"))
                "M&amp;Ms--gotta love 'em")
  (check-equal? (xexpr->markdown '(span (p "Hi.") (p "Hi.")))
                "Hi. Hi.")
  )

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
     ([version "2.0"]
      [xmlns:atom "http://www.w3.org/2005/Atom"])
     (channel
      (title ,(str (current-title) ": " title))
      (description ,(str (current-title) ": " title))
      (link ,(full-uri of-uri-path))
      (atom:link ([href ,(full-uri (abs->rel/www file))]
                  [rel "self"]
                  [type "application/rss+xml"]))
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
    [`(,tag ([,as] ...+) ,es ...)
     `(,tag (,as) ,@(map unlinkify-footnotes/xexpr es))]
    [`(,tag ,es ...)
     `(,tag ,@(map unlinkify-footnotes/xexpr es))]
    [else x]))

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
;; Fucking Google

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

(define (xexpr->string/pretty x)
  (with-output-to-string (thunk (display-xexpr x))))

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
                    [else (void)])])]))
  (fold-files maybe-delete '() (www-path) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-non-post-pages) ;; -> (listof string?)
  (fold-files write-non-post-page '() (src-path) #f))

(define (write-non-post-page path type v)
  (define-values (base name must-be-dir?) (split-path path))
  (cond
   [(and (eq? type 'file)
         (regexp-match? #px"\\.(?:md|markdown)$" path)
         (not (member (path->string name) '("footer.md" "navbar.md")))
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
    (define title (~> path
                      (path-replace-suffix "")
                      file-name-from-path
                      path->string))
    (define uri-path (abs->rel/www dest-path))
    (prn1 "Generating non-post ~a" (abs->rel/top dest-path))
    (~> xs
        (bodies->page #:title title
                      #:description (xexprs->description xs)
                      #:uri-path uri-path)
        xexpr->string/pretty
        (list "<!DOCTYPE html>")
        reverse
        string-join
        string->bytes/utf-8
        (display-to-file* dest-path #:exists 'replace))
    (cons uri-path v)]
   [else v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build)
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
  (write-index posts "All Posts" #f "all"
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
  ;; Generate pygments.css automatically ONLY if it doesn't already
  ;; exist
  (unless (file-exists? (pygments.css))
    (make-pygments.css "default"))
  (prn0 "Done generating files")
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (preview [port 3000])
  (serve/servlet (lambda (_) (next-dispatcher))
                 #:servlet-path "/"
                 #:extra-files-paths (list (www-path))
                 #:port port
                 #:launch-browser? #t
                 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading config file into parameters

(define (get-config name default)
  (define pathname (build-path (top) ".frogrc"))
  (unless (file-exists? pathname)
    (raise-user-error '|Configuration file| "Missing ~a" pathname))
  (let ([v (match (file->string pathname)
             [(pregexp (str "(?:^|\n)"
                            (regexp-quote name) "\\s*=\\s*" "([^#]+?)"
                            "(?:$|\n)")
                       (list _ v)) (maybe-bool v)]
             [else (cond [(procedure? default) (default name)]
                         [else default])])])
    (prn0 ".frogc: ~s = ~s" name v)
    v))

(define (maybe-bool v)
  (match v
    [(or "true" "#t") #t]
    [(or "false" "#f") #f]
    [else v]))

(define (raise-config-required-error name)
  (raise-user-error '|Configuration file|
                    "Missing required item ~s"
                    name))

(require (for-syntax racket/syntax))
(define-syntax (parameterize-from-config stx)
  (syntax-case stx ()
    [(_ ([name default] ...)
        body ...)
     (map identifier? (syntax->list #'(name ...)))
     (with-syntax ([(id ...) (map (lambda (x)
                                    (format-id stx "current-~a" x))
                                  (syntax->list #'(name ...)))]
                   [(key ...) (map (lambda (x)
                                     (symbol->string (syntax-e x)))
                                   (syntax->list #'(name ...)))])
       #'(parameterize ([id (get-config key default)] ...)
           body ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For interactive development
(define (build/preview)
  (parameterize ([top example]
                 [current-verbosity 99])
    (parameterize-from-config ([scheme/host raise-config-required-error]
                               [title "Untitled Site"]
                               [author "The Unknown Author"]
                               [index-full? #f]
                               [feed-full? #f]
                               [google-analytics-account #f]
                               [google-analytics-domain #f]
                               [disqus-shortname #f]
                               [pygments-pathname #f]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f]
                               [older/newer-buttons "both"])
      ;; (clean)
      (build)
      (preview)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (parameterize ([top (current-directory)])
    (parameterize-from-config ([scheme/host raise-config-required-error]
                               [title "Untitled Site"]
                               [author "The Unknown Author"]
                               [index-full? #f]
                               [feed-full? #f]
                               [google-analytics-account #f]
                               [google-analytics-domain #f]
                               [disqus-shortname #f]
                               [pygments-pathname #f]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f]
                               [older/newer-buttons "both"])
      (command-line
       #:once-each
       [("-n" "--new") title
        ("Create a file for a new post based on today's"
         "date and your supplied <title>.")
        (new-post title)]
       [("-m" "--make" "-b" "--build")
        "Generate files."
        (build)]
       [("-p" "--preview")
        "Run a local server and start your browser."
        (preview)]
       [("-c" "--clean")
        "Delete generated files."
        (clean)]
       [("--pygments-css") style-name
        "Generate ./css/pygments.css using style-name (ex: 'default')"
        (make-pygments.css style-name)]
       #:once-any
       [("-v" "--verbose") "Verbose. Put first."
        (current-verbosity 1)
        (prn1 "Verbose mode")]
       [("-V" "--very-verbose") "Very verbose. Put first."
        (current-verbosity 2)
        (prn2 "Very verbose mode")]))))
