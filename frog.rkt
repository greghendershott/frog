#lang rackjure

(require racket/runtime-path
         markdown
         xml
         (prefix-in h: html)
         racket/date
         (only-in srfi/1 break))


(define-runtime-path example "example/") ;; just for dev

;; top is the project directory (e.g. the main dir in Git)
(define top (make-parameter example))

;; sources
(define (src-path) (build-path (top) "_src"))
(define (src/posts-path) (build-path (src-path) "posts"))

;; destinations, from root of the generated web site on down
(define (www-path) (build-path (top)))
(define (www/tags-path) (build-path (www-path) "tags"))
(define (www/feeds-path) (build-path (www-path) "feeds"))

(define (abs->rel/www path)
  (let ([path (path->string path)]
        [root (path->string (www-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) (str "/" x)]
      [else (raise-user-error 'abs->rel/top "root: ~v path: ~v" root path)])))

(define (abs->rel/top path)
  (let ([path (path->string path)]
        [root (path->string (top))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x]
      [else (raise-user-error 'abs->rel/top "root: ~v path: ~v" root path)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-tags (make-hash)) ;; (hashof string? exact-positive-integer?)

(define post-file-px #px"^\\d{4}-\\d{2}-\\d{2}-(.+?)\\.(?:md|markdown)$")

(struct post (title      ;string?
              dest-path  ;path? - full pathname of local HTML file
              uri-path   ;string? - path portion of URI, with leading /
              date       ;string? - 8601
              tags       ;(listof string?)
              blurb      ;(listof xexpr?) - the post summary
              more?      ;boolean? - is `blurb` a subset of `body`?
              body       ;(listof xexpr?) - the full post
              ))

;; Given a uri-path, prepend the schem & host to make a full URI.
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
               (eprintf (str "Skipping ~a\n"
                             "         because it has the tag, 'DRAFT'\n")
                        (abs->rel/top path))
               v]
              [else
               (eprintf "Reading ~a\n" (abs->rel/top path))
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
                           body)
                     v)])]
       [else
        (eprintf (str "Skipping ~a\n"
                      "         Not named ~a\n")
                 (abs->rel/top path)
                 post-file-px)
        v])]
    [else v]))

(define (maybe-make-directory p)
  (unless (directory-exists? p)
    (eprintf "Creating directory ~a\n" (abs->rel/top p))
    (make-directory p)))

(define (write-post-page p older newer)
  (match-define (post title dest-path uri-path date tags blurb more? body) p)
  (eprintf "Generating post ~a\n" (abs->rel/top dest-path))
  (~> (post-xexpr title uri-path date tags body older newer)
      (bodies->page #:title title
                    #:uri-path uri-path)
      xexpr->string
      (list "<!DOCTYPE html>")
      reverse
      string-join
      string->bytes/utf-8
      (display-to-file* dest-path #:exists 'replace)))

(define (post-xexpr title uri-path date tags body older newer)
  `((h1 ,title)
    ,(date+tags->xexpr date tags)
    ,@(filter (negate more-xexpr?) (syntax-highlight-body body))
    (p 'nbsp)
    ,(social uri-path)
    (p 'nbsp)
    ,(older/newer-nav older newer)))

(define (older/newer-nav older newer)
  `(ul ([class "pager"])
       ,(cond [newer
               `(li ([class "previous"])
                    (a ([href ,(post-uri-path newer)])
                       'larr "Newer" 'nbsp (em ,(post-title newer))))]
              [else
               `(li ([class "previous disabled"])
                    (a ([href "#"])
                       'larr "Newer"))])
       ,(cond [older
               `(li ([class "next"])
                    (a ([href ,(post-uri-path older)])
                       (em ,(post-title older)) 'nbsp "Older" 'rarr))]
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

(define (tag->xexpr s [extra ""])
  `(a ([href ,(str "/tags/" (our-encode s) ".html")]) ,s ,extra))

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

;; Bootstrap has some permutations: Response or not, minified or not:
(define responsive? (make-parameter #f)) ;; Responsive not working ???
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
(define (bodies->page xs
                      #:title title
                      #:uri-path uri-path
                      #:feed [feed "all"]) ;; ... -> xexpr?
  (define (toc-xexpr)
    (match (toc xs)
      [`(div ([class "toc"]) (ol ,contents ...))
       (cond [(empty? contents) `(p 'nbsp)]
             [else `(div (p "On this page:"
                            (ol ([class "nav nav-list bs-docs-sidenav"])
                                ,@contents)))])]))
  (define (tag-cloud-xexpr)
    (define alist (~> (for/list ([(k v) (in-hash all-tags)])
                        (cons k v))
                      (sort string-ci<=? #:key car)))
    `(p "Tags:"
        (ul ,@(for/list ([(k v) (in-dict alist)])
                `(li ,(tag->xexpr k (format " (~a)" v)))))))

  `(html ([lang "en"])
         (head (meta ([charset "utf-8"]))
               (title ,title)
               ,(meta "description" title)
               ,(meta "author" (current-author))
               (link ([rel "canonical"][href ,(full-uri uri-path)]))
               (link ([href "favicon.ico"][rel "shortcut icon"]))
               (meta ([name "viewport"]
                      [content "width=device-width, initial-scale=1.0"]))
               ;; CSS
               ,(bootstrap-css)
               ,(link/css "/css/custom.css")
               ,(link/css "/css/pygments.css")
               ;; Atom feed
               (link ([href ,(str "/feeds/" feed ".xml")]
                      [type "application/atom+xml"]
                      [rel "alternate"]
                      [title ,(str (current-title) ": " feed)]))
               ;; JS
               ,(script/js "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js")
               ,(bootstrap-js)
               ,(script/js/inc google-analytics.js
                               (current-google-analytics-account)
                               (current-google-analytics-domain)))
         (body
          (div ([class "navbar"])
               (div ([class "navbar-inner"])
                    (div ([class ,(bs-container)])
                         (ul ([class "nav"])
                             (li ([style "width: 60px;"])
                                 (img ([style "width: 42px; height:33px;"]
                                       [src "/img/gh-head-bw.jpg"])))
                             (li (a ([href "/index.html"][class "brand"])
                                    ,(current-title)))
                             ,(nav-li "/index.html" "Home" uri-path)
                             ;; Perhaps fill this from a Navbar.md file?
                             ,(nav-li "/About.html" "About" uri-path) ))))
          (div ([class ,(bs-container)])
               (div ([class ,(bs-row)])
                    ;; Span2: Docs sidebar
                    (div ([class "span2 bs-docs-sidebar"])
                         ,(toc-xexpr))
                    ;; Span8: Main content
                    (div ([class "span8"])
                         ;; Caller's content
                         ,@xs)
                    ;; Span2: Tags list
                    (div ([class "span2"])
                         ,(tag-cloud-xexpr)))
               (hr)
               (footer
                ,@(with-input-from-file
                      (build-path (src-path) "footer.md")
                    read-markdown)))
          ;; ;; Boostrap JavaScript. Recommended to include at end of body
          ;; ;; body page load speed.
          ;; (script/js "/js/jquery.js")
          ;; (script/js "/js/bootstrap-transition.js")
          ;; (script/js "/js/bootstrap-alert.js")
          ;; (script/js "/js/bootstrap-modal.js")
          ;; (script/js "/js/bootstrap-dropdown.js")
          ;; (script/js "/js/bootstrap-scrollspy.js")
          ;; (script/js "/js/bootstrap-tab.js")
          ;; (script/js "/js/bootstrap-tooltip.js")
          ;; (script/js "/js/bootstrap-popover.js")
          ;; (script/js "/js/bootstrap-button.js")
          ;; (script/js "/js/bootstrap-collapse.js")
          ;; (script/js "/js/bootstrap-carousel.js")
          ;; (script/js "/js/bootstrap-typeahead.js")
          )))

(define (nav-li href text uri-path)
  `(li ,(cond [(string-ci=? href uri-path) `([class "active"])]
              [else `()])
       (a ([href ,href]) ,text)))

(define (social uri-path)
  `(p
    ;; Twitter
    ,(script/js/inc tweet-button.js)
    (a ([href "https://twitter.com/share"]
        [class "twitter-share-button"]
        [data-url ,(full-uri uri-path)]
        [data-dnt "true"])
       "Tweet")
    ,(script/js "https://apis.google.com/js/plusone.js")
    ;; Google+
    (g:plusone ([size "medium"]
                [href ,(full-uri uri-path)]))
    ;; Disqus for comments (but only if (current-disqus-shortname) not #f)
    ,@(cond [(current-disqus-shortname)
             `(,(script/js/inc disqus.js (current-disqus-shortname))
               (div ([id "disqus_thread"])))]
            [else `()])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-index xs title tag feed file) ;; (listof post?) -> any
  (~> (cons
       `(h1 ,@(cond [tag `("Posts tagged " (em ,tag))]
                    [else `(,title)]))
       (for/list ([x (in-list xs)])
        (match-define
         (post title dest-path uri-path date tags blurb more? body) x)
        `(div ([class "index-post"])
              (h2 (a ([href ,uri-path]) ,title))
              ,(date+tags->xexpr date tags)
              ,@blurb
              ,@(cond [more? `((a ([href ,uri-path])
                                  (em "Continue reading ...")))]
                      [else '()]))))
      (add-between `(hr))
      (bodies->page #:title title
                    #:feed feed
                    #:uri-path (abs->rel/www file))
      xexpr->string
      (list "<!DOCTYPE html>\n")
      reverse
      string-join
      string->bytes/utf-8
      (display-to-file* file #:exists 'replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (our-encode s)
  ;; Extremely conservative.
  (~>
   (list->string (for/list ([c (in-string s)])
                   (cond [(or (char-alphabetic? c)
                              (char-numeric? c)) c]
                         [else #\-])))
   (re* #px"-{2,}" "-") ;only one hyphen in a row
   (re #px"-$" "")))    ;no hyphen at end

(define (re* s rx new)
  (regexp-replace* rx s new))
(define (re s rx new)
  (regexp-replace rx s new))

;; Less typing, but also returns its value so good for sticking in ~>
(define (pp v)
  (pretty-print v)
  v)

(define (display-to-file* v path #:exists exists #:mode [mode 'binary])
  (make-directories-if-needed path)
  (display-to-file v path #:exists exists #:mode mode))

(define (make-directories-if-needed path)
  (with-handlers ([exn:fail? (const (void))])
    (define-values (base name dir?)(split-path path))
    (make-directory* base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-highlight-body xs)
  (append* (for/list ([x xs])
             (match x
               [`(pre ([class ,brush]) ,text)
                (match brush
                  [(pregexp "\\s*brush:\\s*(.+?)\\s*$" (list _ lang))
                   (pygmentize text lang)]
                  [else `((pre ,text))])]
               [else (list x)]))))

(define (pygmentize text lang)
  (cond [(pygments-pathname)
         (define tmp-in (make-temporary-file))
         (define tmp-out (make-temporary-file))
         (display-to-file text tmp-in #:exists 'replace)
         (define cmd (str #:sep " "
                          (pygments-pathname)
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

(define (pygments.css)
  (path->string (build-path (www-path) "css" "pygments.css")))

(define (make-pygments.css style)
  (when (pygments-pathname)
    (define cmd (str #:sep " "
                     (pygments-pathname)
                     "-f html"
                     "-S " style
                     "> " (pygments.css)))
    (eprintf "Generating css/pygments.css\n")
    (system cmd)
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-atom-feed xs title of-uri-path file)
  (eprintf "Generating ~a\n" (abs->rel/top file))
  (define updated (post-date (first xs)))
  (~>
   `(feed
     ([xmlns "http://www.w3.org/2005/Atom"]
      [xml:lang "en"])
     (title ([type "text"]) ,(str (current-title) ": " title))
     (link ([rel "self"]
            [href ,(full-uri (abs->rel/www file))]))
     (link ([href ,(full-uri of-uri-path)]))
     ;; (id () ???)
     ;; (etag () ???)
     (updated () ,updated)
     ,@(map post->feed-entry-xexpr xs))
   xexpr->string
   (list "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
   reverse
   string-join
   string->bytes/utf-8
   (display-to-file* file #:exists 'replace)))

(define (post->feed-entry-xexpr x) ;; post? -> xexpr?
  (match-define (post title dest-path uri-path date tags blurb more? body) x)
  `(entry
    ()
    (title ([type "text"]) ,title)
    (link ([rel "alternate"]
           [href ,(full-uri uri-path)]))
    (id () ,title) ;; TODO: some other ID??
    (published () ,date)
    (updated () ,date)
    (content
     ([type "html"])
     ,(xexpr->string
       `(html
         ,@blurb
         ,@(cond [more? `((a ([href ,(full-uri uri-path)])
                             "Continue reading ..."))]
                 [else '()]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define new-post-template
#<<EOF
    Title: ~a
    Date: ~a
    Tags:

_Replace this with your post text. Add one or more comma-separated
tags above._

EOF
)

(define (new-post title)
  (parameterize ([date-display-format 'iso-8601])
    (define d (current-date))
    (define filename (str (~> (str (date->string d #f) ;don't inc time
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
      (eprintf "Deleted ~a\n" (abs->rel/top p)))
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
         (not (member (path->string name) '("footer.md")))
         (not (regexp-match? post-file-px (path->string name))))
    (eprintf "Reading non-post ~a\n" (abs->rel/top path))
    (define xs (with-input-from-file path read-markdown))
    (define dest-path
      (build-path (www-path)
                  (apply build-path
                         (~> path
                             (path-replace-suffix ".html")
                             abs->rel/www
                             explode-path
                             cddr)))) ;lop off the "/" and "_src" parts
    (define title (~> path (path-replace-suffix "") file-name-from-path
                      path->string))
    (define uri-path (abs->rel/www dest-path))
    (eprintf "Generating non-post ~a\n" (abs->rel/top dest-path))
    (~> xs
        (bodies->page #:title title
                      #:uri-path uri-path)
        xexpr->string
        (display-to-file* dest-path #:exists 'replace))
    (cons uri-path v)]
   [else v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build)
  (define (post<=? a b)
    (string<=? (post-date a) (post-date b)))

  (set! all-tags (make-hash))

  ;; Read the posts and get (listof post?) with which to do more work.
  ;; Sort the list by date, newest first.
  (define xs (~> (fold-files read-post '() (src/posts-path) #f)
                 (sort (negate post<=?))))
  ;; Make lists for older and newer items. Just shift each list and
  ;; add #f to one end.
  (define older (append (drop xs 1) (list #f)))
  (define newer (cons #f (take xs (sub1 (length xs)))))
  ;; Write the post pages
  (for-each write-post-page xs older newer)
  ;; For each tag, write an index page and Atom feed
  (for ([(tag _) (in-hash all-tags)])
    (define xs-this-tag (filter (lambda (x)
                                  (member tag (post-tags x)))
                                xs))
    (define tag-index-path
      (build-path (www/tags-path) (str (our-encode tag) ".html")))
    (write-index xs-this-tag
                 (str "Posts tagged '" tag "'")
                 tag
                 (our-encode tag)
                 tag-index-path)
    (write-atom-feed xs-this-tag
                     (str "Posts tagged '" tag "'")
                     (abs->rel/www tag-index-path)
                     (build-path (www/feeds-path) (str (our-encode tag)
                                                       ".xml"))))
  ;; Write the index page for all posts
  (write-index xs "All Posts" #f "all" (build-path (www-path) "index.html"))
  ;; Write Atom feed for all posts
  (write-atom-feed xs "All Posts" "/index.html"
                   (build-path (www/feeds-path) "all.xml"))
  ;; Generate non-post pages.
  (define npps (build-non-post-pages))
  ;; Write sitemap
  ;; One gotcha: What about other "sub-sites", for example GitHub project
  ;; pages?  How to include them in sitemap.txt?
  (eprintf "Generating sitemap\n")
  (with-output-to-file (build-path (www-path) "sitemap.txt")
    #:exists 'replace
    (thunk (for ([x (in-list (map full-uri
                                  (append (map post-uri-path xs) npps)))])
             (displayln x))))
  ;; Generate pygments.css automatically ONLY if it doesn't already
  ;; exist
  (unless (file-exists? (pygments.css))
    (make-pygments.css "default"))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in serve: lobe/serve-files)
         net/sendurl)

(define (preview [port 3000])
  (printf "Start preview web server at localhost:~a\n" port)
  (define stop
    (parameterize ([serve:home (www-path)]
                   [serve:port port]
                   [serve:dotfiles? #f])
      (serve:start)))
  (printf "Open web browser on http://localhost:~a/index.html\n" port)
  (send-url (format "http://localhost:~a/index.html" port))
  (displayln "Server running. Press CTRL-C to stop it...")
  (with-handlers ([exn? (lambda (exn)
                          (stop)
                          (displayln "\nServer stopped."))])
    (sync never-evt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These should probably be read from a per-project `.frogrc`?

(define current-scheme/host (make-parameter "http://www.greghendershott.com"))

(define current-title (make-parameter "Greg Hendershott"))
(define current-author (make-parameter "Greg Hendershott"))

(define current-google-analytics-account (make-parameter "UA-29709446-1"))
(define current-google-analytics-domain (make-parameter "greghendershott.com"))

(define current-disqus-shortname (make-parameter "greghendershott"))

(define pygments-pathname
  (make-parameter (expand-user-path "~/src/python/pygments-main/pygmentize")))
(define pygments-style (make-parameter "default"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Can uncomment these during development -- but don't commit that way!

#|

(begin
  (clean)
  (build)
  (preview))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (parameterize ([top (current-directory)])
    (command-line
     #:once-each
     [("-c" "--clean")
      "Delete generated files."
      (clean)]
     [("-m" "--make" "-b" "--build")
      "Generate files."
      (build)]
     [("-p" "--preview")
      "Run a local server and starting your browser."
      (preview)]
     [("-n" "--new") title
      ("Create a file for a new post based on today's"
       "date and your supplied <title>.")
      (new-post title)]
     [("--pygments-css") style-name
      "Generate ./css/pygments.css using style-name (ex: 'default')"
      (make-pygments.css style-name)])))

#|

TODO:

- Index pages: Paingate, limiting to N posts per page, with
  Older/Newer nav.

- Top navbar. Including non-post pages; see previous item.

- Get the responsive stuff working.


DONE:

- Post pages: Add Older/Newer nav.

- Index pages: Sort posts in reverse date order.

- DRAFT tag to exclude from generation.

- Feeds for every tag.

- Non-post pages (e.g. About, whatever) authored in
  Markdown. (Currently can plop any old HTML in WWW, but, (a) it won't
  use the master page of bodies->page and (b) it won't be auto-linked.

- Share buttons (at least mailto:, Twitter and Google+).

- sitemap.txt

|#
