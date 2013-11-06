#lang rackjure  ;; dependency 1 of 2

(require markdown ;; dependency 2 of 2
         racket/runtime-path
         xml
         (only-in html read-html-as-xml)
         net/uri-codec
         net/url
         json
         racket/date
         (only-in find-parent-dir find-parent-containing)
         (only-in srfi/1 break)
         (for-syntax racket/syntax)
         "config.rkt"
         "doc-uri.rkt"
         "feeds.rkt"
         "params.rkt"
         "paths.rkt"
         "post.rkt"
         "pygments.rkt"
         "scribble.rkt"
         "take.rkt"
         "template.rkt"
         "watch-dir.rkt"
         "xexpr2text.rkt"
         "xexpr-map.rkt"
         "util.rkt"
         "verbosity.rkt"
         ;; Remainder are just for the serve/preview feature:
         web-server/servlet-env
         web-server/http
         web-server/dispatchers/dispatch)

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tags found among all posts, and how many times.
(define all-tags (make-hash)) ;(hash/c string? exact-positive-integer?)

(define post-file-px
  #px"^(\\d{4}-\\d{2}-\\d{2})-(.+?)\\.(?:md|markdown|scrbl)$")

;; A function to provide to `fold-files`.
(define (read-post path type v)
  (cond
    [(eq? type 'file)
     (define-values (base name must-be-dir?) (split-path path))
     (match (path->string name)
       [(pregexp post-file-px (list _ dt nm))
        ;; Read either Markdown or Scribble file
        (prn1 "Reading ~a" (abs->rel/top path))
        (define xs
          (match (path->string name)
            [(pregexp "\\.scrbl$")
             (define img-dest (build-path (www/img-path)
                                          "posts"
                                          (str dt "-" nm)))
             (read-scribble-file path
                                 #:img-local-path img-dest
                                 #:img-uri-prefix (abs->rel/www img-dest))]
            [_
             ;; Footnote prefix is date & name w/o ext
             ;; e.g. "2010-01-02-a-name"
             (define footnote-prefix (~> (str dt "-" nm) string->symbol))
             (with-input-from-file path
               (lambda () (read-markdown footnote-prefix)))]))
        ;; Split to the meta-data and the body
        (define-values (title date tags body) (meta-data xs))
        (cond [(member "DRAFT" tags)
               (prn0 "        Skipping because it has the tag, 'DRAFT'")
               v]
              [else
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
  (define (err x)
    (raise-user-error 'error
                      "Post must start with Title/Date/Tags, but found:\n~v"
                      x))
  (define px "^Title:\\s*(.+?)\nDate:\\s*(.+?)\nTags:\\s*(.*?)\n*$")
  (match xs
    [`(,(or `(pre ,metas ...)           ;Markdown
            `(p () ,metas ...))         ;Scribble
       ,more ...)
     ;; In the meta-data we don't want HTML entities like &ndash; we
     ;; want plain text.
     (match (string-join (map xexpr->markdown metas))
       [(pregexp px (list _ title date tags))
        (values title date (tag-string->tags tags) more)]
       [_ (err (first xs))])]
    [(list x _ ...) (err x)]
    [_ (err "")]))

(module+ test
  (check-not-exn (thunk (meta-data `((pre "Title: title\nDate: date\nTags: DRAFT\n")))))
  (check-not-exn (thunk (meta-data `((p () "Title: title" ndash "hyphen \nDate: date\nTags: DRAFT\n\n")))))
  (check-exn exn? (thunk (meta-data '((pre "not meta data")))))
  (check-exn exn? (thunk (meta-data '((p () "not meta data"))))))

(define (tag-string->tags s)
  (~>> (regexp-split #px"," s)
       (map string-trim)))

(module+ test
  (check-equal? (tag-string->tags " some, post ,   tags ")
                '("some" "post" "tags")))

(define (above-the-fold xs)
  (define-values (above below) (break more-xexpr? xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(p ,(pregexp "\\s*<!--\\s*more\\s*-->\\s*")) #t]             ;Markdown
    [`(p () "<" "!" ndash ,(pregexp "\\s*more\\s*") ndash ">") #t] ;Scribble
    [_ #f]))

(module+ test
  (check-true (more-xexpr? `(p   "<!--more-->")))
  (check-true (more-xexpr? `(p " <!-- more -->")))
  (check-true (more-xexpr? `(p "<!--  more  -->")))
  (check-true (more-xexpr? `(p () "<" "!" ndash   "more"   ndash ">")))
  (check-true (more-xexpr? `(p () "<" "!" ndash  " more "  ndash ">")))
  (check-true (more-xexpr? `(p () "<" "!" ndash "  more  " ndash ">")))
  (check-false (more-xexpr? "not more")))

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
        'content (~> body enhance-body xexprs->string)
        'older-uri (and older (post-uri-path older))
        'newer-uri (and newer (post-uri-path newer))
        'older-title (and older (post-title older))
        'newer-title (and newer (post-title newer))})
      ;; bodies->page wants (listof xexpr?) so convert from string? to that
      string->xexpr
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
    'tags-list-items (xexprs->string (tags-list-items))
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
  `((p "Tags:"
       (ul ,@(for/list ([(k v) (in-dict (tags-alist))])
               `(li ,(tag->xexpr k)
                    nbsp
                    ,@(if (current-show-tag-counts?) `(,(format "(~a)" v)) '())
                    " "
                    (a ([href ,(atom-feed-uri k)])
                       (img ([src "/img/feed.png"]))))))
    (p (a ([href ,(current-posts-index-uri)]) "All Posts")
       " "
       (a ([href ,(atom-feed-uri "all")])
          (img ([src "/img/feed.png"])))))))

(define (tags-list-items)
  (for/list ([(k v) (in-dict (tags-alist))])
               `(li ,(tag->xexpr k))))

(define (tags-alist)
  ;; Sort alphabetically by tag name. Use association list (can't sort
  ;; a hash).
  (~> (for/list ([(k v) (in-hash all-tags)])
        (cons k v))
      (sort string-ci<=? #:key car)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-index-pages xs    ;(listof post?) -> any
                           title ;string?
                           tag   ;(or/c #f string?)
                           feed  ;string?
                           file) ;path?
  (define num-posts (length xs))
  (define num-pages (ceiling (/ num-posts (current-posts-per-page))))
  (for ([page-num (in-range num-pages)]
        [page-posts (in-list (take-every xs (current-posts-per-page)))])
    (write-index-page page-posts
                      (str title " (page " (add1 page-num) ")")
                      tag
                      feed
                      file
                      page-num
                      num-pages)))

(define (write-index-page xs         ;(listof post?) -> any
                          title      ;string?
                          tag        ;(or/c #f string?)
                          feed       ;string?
                          base-file  ;path?
                          page-num   ;exact-positive-integer?
                          num-pages) ;exact-positive-integer?
  (define file (file/page base-file page-num))
  (prn1 "Generating ~a" (abs->rel/top file))
  (~> (for/list ([x (in-list xs)])
        (match-define
         (post title dest-path uri-path date tags blurb more? body) x)
        `(article
          ([class "index-post"])
          (header (h2 (a ([href ,uri-path]) ,title))
                  ,(date+tags->xexpr date tags))
          (div ([class "entry-content"])
               ,@(cond [(current-index-full?) (enhance-body body)]
                       [more? `(,@(enhance-body blurb)
                                (footer ([class "read-more"])
                                 (a ([href ,uri-path]) hellip "more" hellip)))]
                       [else (enhance-body blurb)]))))
      (append `((footer ,(bootstrap-pagination base-file page-num num-pages))))
      (bodies->page #:title title
                    #:description title
                    #:feed feed
                    #:uri-path (abs->rel/www file)
                    #:keywords (cond [tag (list tag)]
                                     [else (hash-keys all-tags)])
                    #:tag tag
                    #:toc-sidebar? tag) ;; no toc on home page
      (display-to-file* file #:exists 'replace)))

(define (bootstrap-pagination base-file page-num num-pages)
  `(ul ([class "pagination"])
       ,(cond [(zero? page-num) `(li ([class "disabled"])
                                     (a ([href "#"]) 'larr))]
              [else `(li (a ([href ,(~> (file/page base-file (sub1 page-num))
                                        abs->rel/www)])
                            'larr))])
       ,@(for/list ([n (in-range num-pages)])
           `(li (,@(cond [(= n page-num) `([class "active"])] [else '()]))
                (a ([href ,(~> (file/page base-file n) abs->rel/www)])
                   ,(number->string (add1 n)))))
       ,(cond [(= (add1 page-num) num-pages) `(li ([class "disabled"])
                                                  (a ([href "#"]) 'rarr))]
              [else `(li (a ([href ,(~> (file/page base-file (add1 page-num))
                                        abs->rel/www)])
                            'rarr))]) ))

(define (file/page base-file page-num)
  (cond [(zero? page-num) base-file]
        [else (~> base-file             ;add "-<page>" suffix
                  (path-replace-suffix "")
                  path->string
                  (str "-" (add1 page-num))
                  string->path
                  (path-replace-suffix ".html"))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enhance-body xs)
  (~> xs
      syntax-highlight
      add-racket-doc-links
      auto-embed-tweets))

(define (syntax-highlight xs)
  (for/list ([x xs])
    (match x
      [`(pre ([class ,brush]) ,text)
       (match brush
         [(pregexp "\\s*brush:\\s*(.+?)\\s*$" (list _ lang))
          `(div ([class ,(str "brush: " lang)])
                ,@(pygmentize text lang))]
         [_ `(pre ,text)])]
      [_ x])))

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
                      `(code ([class "brush: racket"]) ,xs ...))
                     (if (current-racket-doc-link-prose?)
                         `(code () ,@(->racket-doc-links xs))
                         x)]
                    ;; Only spans from Pygments lexed as Racket
                    [(`((pre ,_ ...)
                        (div ,_ ...)
                        (td ,_ ...)
                        (tr ,_ ...)
                        (tbody ,_ ...)
                        (table ,_ ...)
                        (div ([class "brush: racket"]) ,_ ...))
                      `(span ([class ,c]) ,xs ...))
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
                ,_ ...))
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
         (cond [html (~>> (with-input-from-string html read-html-as-xml)
                          (element #f #f
                                   'div `(,(attribute #f #f
                                                      'class "embed-tweet")))
                          xml->xexpr)]
               [else x])]
        [_ x])))
  (cond [(current-auto-embed-tweets?) (do-it xs)]
        [else xs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define new-markdown-post-template
#<<EOF
    Title: ~a
    Date: ~a
    Tags: DRAFT

_Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published._

<!-- more -->

EOF
)

(define new-scribble-post-template
#<<EOF
#lang scribble/manual

Title: ~a
Date: ~a
Tags: DRAFT

Replace this with your post text. Add one or more comma-separated
Tags above. The special tag `DRAFT` will prevent the post from being
published.

<!-- more -->

EOF
)

(define (new-post title (type 'markdown))
  (let ([extension (case type
                     [(markdown) ".md"]
                     [(scribble) ".scrbl"])]
        [template  (case type
                     [(markdown) new-markdown-post-template]
                     [(scribble) new-scribble-post-template])])
    (parameterize ([date-display-format 'iso-8601])
      (define d (current-date))
      (define filename (str (~> (str (date->string d #f) ;omit time
                                     "-"
                                     (~> title string-downcase))
                                our-encode)
                            extension))
      (define pathname (build-path (src/posts-path) filename))
      (when (file-exists? pathname)
        (raise-user-error 'new-post "~a already exists." pathname))
      (display-to-file* (format template
                                title
                                (date->string d #t)) ;do includde time
                        pathname
                        #:exists 'error)
      (displayln pathname)
      ;; (define editor (getenv "EDITOR"))
      ;; (when editor
      ;;   (system (format "~a ~a &" editor (path->string pathname))))
      )))

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
      (cond [(equal? path (reroot-path (current-posts-index-uri) (www-path)))
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

(define (build-non-post-pages) ;; -> (listof string?)
  (fold-files write-non-post-page '() (src-path) #f))

(define (write-non-post-page path type v)
  (define-values (base name must-be-dir?) (split-path path))
  (cond
   [(and (eq? type 'file)
         (regexp-match? #px"\\.(?:md|markdown|scrbl)$" path)
         (not (regexp-match? post-file-px (path->string name))))
    (prn1 "Reading non-post ~a" (abs->rel/top path))
    (define dest-path
      (build-path (www-path)
                  (apply build-path
                         (~> path
                             (path-replace-suffix ".html")
                             abs->rel/www
                             explode-path
                             cddr)))) ;lop off the "/" and "_src" parts
    (define xs
      (~> (match (path->string name)
            [(pregexp "\\.scrbl$")
             (define img-dest (path-replace-suffix dest-path ""))
             (read-scribble-file path
                                 #:img-local-path img-dest
                                 #:img-uri-prefix (abs->rel/www img-dest))]
            [_ (with-input-from-file path read-markdown)])
          enhance-body))
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
  ;; Sort the list by date.
  (define (post<=? a b)
    (cond [(current-index-newest-first?)
           (string<=? (post-date a) (post-date b))]
          [else
           (string<=? (post-date b) (post-date a))]))
  (set! all-tags (make-hash))
  (define posts (~> (fold-files* read-post '() (src/posts-path) #f)
                    (sort (negate post<=?))))
  (define-values (older newer) ;; older/newer posts
    (match posts
      ['() (values '() '())]
      [_   (values (append (drop posts 1) (list #f))
                   (cons #f (take posts (sub1 (length posts)))))]))
  ;; Write the post pages
  (for-each write-post-page posts older newer)
  ;; For each tag, write an index page, Atom feed, RSS feed
  (define (post-has-tag? tag post)
    (member tag (post-tags post)))
  (for ([(tag _) (in-hash all-tags)])
    (define posts-this-tag (filter (curry post-has-tag? tag) posts))
    (define tag-index-path
      (build-path (www/tags-path) (str (our-encode tag) ".html")))
    (write-index-pages posts-this-tag
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
  (write-index-pages posts (current-title) #f "all"
                     (reroot-path (current-posts-index-uri) (www-path)))
  ;; Write Atom feed for all posts
  (write-atom-feed posts "All Posts" "all" (current-posts-index-uri)
                   (build-path (www/feeds-path) "all.atom.xml"))
  ;; Write RSS feed for all posts
  (write-rss-feed posts "All Posts" "all" (current-posts-index-uri)
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
    (prn0 (~a "Done generating files at " (n date-hour) ":" (n date-minute))))
  (void))

;; Like `fold-files`, but if `start-path` is not #f and does not exist,
;; this returns `init-val` rather than abending with "path
;; disappeared" error.
(define (fold-files* proc init-val [start-path #f] [follow-links? #f])
  (cond [(and start-path (not (directory-exists? start-path))) init-val]
        [else (fold-files proc init-val start-path follow-links?)]))

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

(define (serve #:launch-browser? launch-browser?
               #:watch? watch?
               #:port port)
  (define watcher-thread
    (cond [watch? (watch-directory (build-path (top))
                     '(file)
                     (lambda (path what)
                       (match (path->string path)
                         ;; Output file
                         [(pregexp "\\.(?:html|xml|txt)") (void)]
                         ;; Source file
                         [_ (unless (build-one path)
                              (build))
                            (displayln #"\007")])) ;beep (hopefully)
                     #:rate 5)]
          [else (thread (thunk (sync never-evt)))]))
  (when launch-browser?
    (ensure-external-browser-preference))
  (serve/servlet (lambda (_) (next-dispatcher))
                 #:servlet-path "/"
                 #:extra-files-paths (list (www-path))
                 #:port port
                 #:listen-ip #f
                 #:launch-browser? launch-browser?)
  (kill-thread watcher-thread))

(define (ensure-external-browser-preference)
  ;; `serve/servlet` uses the `send-url` from `net/sendurl`, which
  ;; (unlike the `send-url` from `external/browser`) doesn't prompt
  ;; the user if no external browser preference is set. This can
  ;; matter on some Linux distributions, e.g. Ubuntu which needs
  ;; xdg-open or sensible-browser, but `net/sendurl` uses neither and
  ;; doesn't even prompt the user. So check for this case here, and if
  ;; no browser preference set yet, ask the user, like the `send-url`
  ;; from `external/browser` would do.
  (when (eq? 'unix (system-type 'os))
    (unless (get-preference 'external-browser)
      ((dynamic-require 'browser/external
                        'update-browser-preference) #f))))

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
                               [max-feed-items 999]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f]
                               [auto-embed-tweets? #t]
                               [racket-doc-link-code? #t]
                               [racket-doc-link-prose? #f]
                               [posts-per-page 2] ;small, for testing
                               [index-newest-first? #t]
                               [posts-index-uri "/index.html"])
      ;; (clean)
      (build)
      (serve #:launch-browser? #f
             #:watch? #f
             #:port 3000)
      ;; (watch)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-frog-root)
  (or (find-parent-containing (current-directory) ".frogrc")
      (current-directory)))

(module+ main
  (require "../info.rkt")
  (printf "Frog ~a\n" (#%info-lookup 'version))
  (parameterize* ([top (find-frog-root)])
    (parameterize-from-config (build-path (top) ".frogrc")
                              ([scheme/host "http://www.example.com"]
                               [title "Untitled Site"]
                               [author "The Unknown Author"]
                               [show-tag-counts? #t]
                               [permalink "/{year}/{month}/{title}.html"]
                               [index-full? #f]
                               [feed-full? #f]
                               [max-feed-items 999]
                               [decorate-feed-uris? #t]
                               [feed-image-bugs? #f]
                               [auto-embed-tweets? #t]
                               [racket-doc-link-code? #t]
                               [racket-doc-link-prose? #f]
                               [posts-per-page 10]
                               [index-newest-first? #t]
                               [posts-index-uri "/index.html"])
      (define watch? #f)
      (define port 3000)
      (command-line
       #:program "frog"
       #:once-each
       [("--init")
        (""
         "Initialize current directory as a new Frog project, creating"
         "default files as a starting point.")
        (init-project)]
       #:multi
       [("-n" "--new") title
        (""
         "Create a .md file for a new post based on today's date and <title>.")
        (new-post title 'markdown)]
       [("-N" "--new-scribble") title
        (""
         "Create a .scrbl file for a new post based on today's date and <title>.")
        (new-post title 'scribble)]
       [("-b" "--build")
        (""
         "Generate files.")
        (build)]
       [("-c" "--clean")
        (""
         "Delete generated files.")
        (clean)]
       #:once-each
       [("-w" "--watch")
        (""
         "(Experimental: Only rebuilds some files.)"
         "Supply this flag before -s/--serve or -p/--preview."
         "Watch for changed files, and generate again."
         "(You'll need to refresh the browser yourself.")
        (set! watch? #t)]
       [("--port") number
        (""
         "The port number for -s/--serve or -p/--preview."
         "Supply this flag before one of those flags."
         "Default: 3000.")
        (set! port (string->number number))]
       #:once-any
       [("-s" "--serve")
        (""
         "Run a local web server.")
        (serve #:launch-browser? #f
               #:watch? watch?
               #:port port)]
       [("-p" "--preview")
        (""
         "Run a local web server and start your browser on blog home page.")
        (serve #:launch-browser? #t
               #:watch? watch?
               #:port port)]
       #:once-any
       [("-v" "--verbose") "Verbose. Put first."
        (current-verbosity 1)
        (prn1 "Verbose mode")]
       [("-V" "--very-verbose") "Very verbose. Put first."
        (current-verbosity 2)
        (prn2 "Very verbose mode")]))))
