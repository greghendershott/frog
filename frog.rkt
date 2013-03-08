#lang rackjure

(require racket/runtime-path
         markdown
         xml
         racket/date
         (only-in srfi/1 break))

;; top is the source directory
(define-runtime-path example "example")
(define top (make-parameter example))

;; sources
(define (src-path) (build-path (top) "src"))
(define (src/posts-path) (build-path (top) "src" "posts"))

;; destinations, from root of the generated web site on down
(define (www-path) (build-path (top) "www"))
(define (www/tags-path) (build-path (top) "www" "tags"))

(define all-tags (make-hash)) ;; (hashof string? exact-positive-integer?)

(struct post (title dest-path uri date tags blurb more? body))

;; A function for fold-files
(define (read-post path type v)
  (cond
    [(eq? type 'file)
     (define-values (base name must-be-dir?) (split-path path))
     (match (path->string name)
       [(pregexp "^\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-.+?\\.(?:md|markdown)$")
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
               (define dest-path (~> (build-path (www-path) name)
                                     (path-replace-suffix ".html")))
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
        (eprintf
         (str "Skipping ~a\n"
              "         Not named YYYY-MM-DD-HH-MM-SS-TITLE.{md,markdown}\n")
         (abs->rel/top path))
        v])]
    [else v]))

(define (write-post-page p older newer)
  (match-define (post title dest-path uri date tags blurb more? body) p)
  (eprintf "Generating ~a\n" (abs->rel/top dest-path))
  (~> (post-xexpr title date tags body older newer)
      (bodies->page #:title title)
      xexpr->string
      (display-to-file dest-path #:exists 'replace)))

(define (post-xexpr title date tags body older newer)
  `((h1 ,title)
    (p ,date)
    ,(tags->xexpr tags)
    ,@(filter (negate more?) body)
    ,(older/newer-nav older newer)))

(define (older/newer-nav older newer)
  `(ul ([class "pager"])
       ,(cond [newer
               `(li ([class "previous"])
                    (a ([href ,(post-uri newer)])
                       'larr "Newer" 'nbsp (em ,(post-title newer))))]
              [else
               `(li ([class "previous disabled"])
                    (a ([href "#"])
                       'larr "Newer"))])
       ,(cond [older
               `(li ([class "next"])
                    (a ([href ,(post-uri older)])
                       (em ,(post-title older)) 'nbsp "Older" 'rarr))]
              [else
               `(li ([class "next disabled"])
                    (a ([href "#"])
                       "Older" 'rarr))])))

(define (meta-data xs)
  (match (first xs)
    [(list 'pre (list 'code s))
     (match s
       [(pregexp "^Title: (.+?)\nDate: (.+?)\nTags:\\s*(.*?)\n*$"
                 (list _ title date tags))
        (values title date (tag-string->tags tags) (rest xs))])]))

(define (tag->xexpr s [extra ""])
  `(a ([href ,(str "/tags/" (our-encode s) ".html")]) ,s ,extra))

(define (tags->xexpr xs)
  (cond [(empty? xs) '()]
        [else
         `(p "Tags: "
             ,@(add-between (map tag->xexpr xs)
                            ", "))]))

(define (tag-string->tags s)
  (regexp-split #px",\\s*" s))

(define (above-the-fold xs)
  (define-values (above below) (break more? xs))
  (values above (not (empty? below))))

(define (more? x)
  (match x
    [(list p "<!-- more -->") #t]
    [else #f]))

(define (abs->rel/www path)
  (let ([path (path->string path)]
        [root (path->string (www-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x])))

(define (abs->rel/top path)
  (let ([path (path->string path)]
        [root (path->string (top))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "/(.+$)") (list _ x)) x])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bodies->page
;;
;; This is really a "master page" or "site template". It puts the body
;; elements in a container div.

(define-runtime-path
  google-analytics-template.js
  "google-analytics-template.js")

(define responsive? (make-parameter #f)) ;; Why isn't this working ???
(define minified? (make-parameter #t))
(define (bs-js)
  (if (minified?) "/js/bootstrap.js" "/js/bootstrap.min.js"))
(define (bs-css)
  (cond [(minified?)
         (cond [(responsive?) "/css/bootstrap-responsive.min.css"]
               [else "/css/bootstrap.min.css"])]
        [else
         (cond [(responsive?) "/css/bootstrap-responsive.css"]
               [else "/css/bootstrap.css"])]))
(define (bs-container)
  (if (responsive?) "container-fluid" "container"))
(define (bs-row)
  (if (responsive?) "row-fluid" "row"))

(define (bodies->page xs
                      #:title title
                      #:feed [feed "all"]
                      #:newer [newer #f]
                      #:older [older #f]) ;; ... -> xexpr?
  (define (toc-xexpr)
    (match (toc xs)
      [`(div ([class "toc"]) (ol ,contents ...))
       (cond [(empty? contents) ""]
             [else `(div (hr)
                         (p "On this page:"
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
               (meta ([name "description"][content ,title]))
               (meta ([name "author"][content ,(current-author)]))
               (link ([href "favicon.ico"][rel "shortcut icon"]))
               (meta ([name "viewport"]
                      [content "width=device-width, initial-scale=1.0"]))
               ;; CSS
               (link ([href ,(bs-css)][rel "stylesheet"][media "screen"]))
               (link ([href "/css/custom.css"][rel "stylesheet"][type "text/css"]))
               ;; Atom feed
               (link ([href ,(str "/feeds/" feed ".xml")]
                      [type "application/atom+xml"]
                      [rel "alternate"]
                      [title ,(str (current-title) ": " feed)]))
               ;; JS
               (script ([src "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"]))
               (script ([src ,(bs-js)]))
               (script ([type "text/javascript"])
                       ,(format (file->string google-analytics-template.js)
                                (current-google-analytics-account)
                                (current-google-analytics-domain))))

         (body (div ([class "navbar navbar-fixed-top"])
                    (div ([class "navbar-inner"])
                         (div ([class ,(bs-container)])
                              (p ([class "pull-left"])
                                 (img ([style "width: 42px; height:33px;"]
                                       [src "/img/gh-head-bw.jpg"]) 'nbsp))
                              (a ([class "brand"]
                                  [href="#"])
                                 ,(current-title)))))

               (div ([class ,(bs-container)])

                    (div ([class ,(bs-row)])
                         ;; Docs sidebar
                         (div ([class "span2 bs-docs-sidebar"])
                              ,(tag-cloud-xexpr)
                              ,(toc-xexpr))
                         ;; Main content passed to bodies->page
                         (div ([class "span8"])
                              ,@xs))

                    (hr)
                    (footer
                     ,@(with-input-from-file (build-path (src-path) "footer.md")
                         read-markdown))

                    ;; Bootstrap JS (optional)
                    ;; (script ([src "js/jquery.js"]))
                    ;; (script ([src "js/bootstrap-transition.js"]))
                    ;; (script ([src "js/bootstrap-alert.js"]))
                    ;; (script ([src "js/bootstrap-modal.js"]))
                    ;; (script ([src "js/bootstrap-dropdown.js"]))
                    ;; (script ([src "js/bootstrap-scrollspy.js"]))
                    ;; (script ([src "js/bootstrap-tab.js"]))
                    ;; (script ([src "js/bootstrap-tooltip.js"]))
                    ;; (script ([src "js/bootstrap-popover.js"]))
                    ;; (script ([src "js/bootstrap-button.js"]))
                    ;; (script ([src "js/bootstrap-collapse.js"]))
                    ;; (script ([src "js/bootstrap-carousel.js"]))
                    ;; (script ([src "js/bootstrap-typeahead.js"]))

                    ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-index xs title feed file) ;; (listof post?) -> any
  (define (string-prepend s prepend)
    (string-append prepend s))
  (~> (cons
       `(h1 ,title)
       (for/list ([x (in-list xs)])
        (match-define (post title dest-path uri date tags blurb more? body) x)
        `(div ([class "index-post"])
              (h1 (a ([href ,uri]) ,title))
              (p ,date)
              ,(tags->xexpr tags)
              ,@blurb
              ,@(cond [more? `((a ([href ,uri])
                                  (em "Continue reading ...")))]
                      [else '()]))))
      (add-between `(hr))
      (bodies->page #:title title #:feed feed)
      xexpr->string
      (string-prepend "<!DOCTYPE html>\n")
      (display-to-file file #:exists 'replace)))

(define (our-encode s)
  ;; Extremely conservative.
  (list->string (for/list ([c (in-string s)])
                  (cond [(or (char-alphabetic? c)
                             (char-numeric? c)) c]
                        [else #\-]))))

;; Less typing, but also returns its value so good for sticking in ~>
(define (pp v)
  (pretty-print v)
  v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-atom-feed xs title file)
  (define x
    `(feed
      ([xmlns "http://www.w3.org/2005/Atom"]
       [xml:lang "en"])
      (title ([type "text"]) ,(str (current-title) ": " title))
      ;; (link ([href ,(gplus-self-uri g)]
      ;;        [rel "self"]))
      ;; (link ([href ,(gplus-self-uri g)]))
      ;; (id () ,(gplus-id g))
      ;; (etag () ,(gplus-etag g))
      ;; (updated () ,(gplus-updated g))
      ,@(map post->feed-entry-xexpr xs)))
  (~> x
      xexpr->string
      (display-to-file file #:exists 'replace)))

(define (post->feed-entry-xexpr x) ;; post? -> xexpr?
  (match-define (post title dest-path uri date tags blurb more? body) x)
  `(entry
    ()
    (title ([type "text"]) ,title)
    (link ([rel "alternate"]
           [href ,uri]))
    (id () ,title) ;; TODO: some other ID??
    (published () ,date)
    (updated () ,date)
    (content
     ([type "html"])
     ,(xexpr->string
       `(html
         ,@blurb
         ,@(cond [more? `((a ([href ,uri])
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
  (define date-str (parameterize ([date-display-format 'iso-8601])
                     (date->string (current-date) #t)))
  (define filename (str (~> (str (regexp-replace "T" date-str "-")
                                 "-"
                                 (~> title string-downcase))
                            our-encode)
                        ".md"))
  (define pathname (build-path (src/posts-path) filename))
  (when (file-exists? pathname)
    (raise-user-error 'new-post "~a already exists." pathname))
  (display-to-file (format new-post-template
                           title date-str)
                   pathname
                   #:exists 'error)
  (displayln pathname)
  ;; (define editor (getenv "EDITOR"))
  ;; (when editor
  ;;   (system (format "~a ~a &" editor (path->string pathname))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            [else (match (path->string name)
                    [(pregexp
                      "^\\d{4}-\\d{2}-\\d{2}t\\d{2}-\\d{2}-\\d{2}-.+?\\.html$")
                     (rm path)]
                    [else (void)])])]))
  (fold-files maybe-delete '() (www-path) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build)
  (define (post<=? a b)
    (string<=? (post-date a) (post-date b)))

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
    (write-index
     xs-this-tag
     (str "Posts tagged '" tag "'")
     (our-encode tag)
     (build-path (www-path) "tags" (str (our-encode tag) ".html")))
    (write-atom-feed
     xs-this-tag
     (str "Posts tagged '" tag "'")
     (build-path (www-path) "feeds" (str (our-encode tag) ".xml"))))
  ;; Write the index page for all posts
  (write-index xs "All Posts" "all" (build-path (www-path) "index.html"))
  ;; Write Atom feed for all posts
  (write-atom-feed xs "All Posts" (build-path (www-path) "feeds" "all.xml")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in serve: lobe/serve-files)
         net/sendurl)

(define (preview)
  (define port 3000)
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

(define current-title (make-parameter "Greg Hendershott"))
(define current-author (make-parameter "Greg Hendershott"))

(define current-google-analytics-account (make-parameter "UA-29709446-1"))
(define current-google-analytics-domain (make-parameter "greghendershott.com"))

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
  (define clean? #f)
  (define build? #f)
  (define preview? #f)
  (command-line
   #:once-each
   [("-c" "--clean")
    "Delete generated files."
    (set! clean? #t)]
   [("-m" "--make" "-b" "--build")
    "Generate files."
    (set! build? #t)]
   [("-p" "--preview")
    "Run a local server and starting your browser."
    (set! preview? #t)]
   [("-n" "--new") title
    ("Create a file for a new post based on today's"
    "date and your supplied <title>.")
    (new-post title)])
  (when clean? (clean))
  (when build? (build))
  (when preview? (preview)))


#|

TODO:

- Index pages: Paingate, limiting to N posts per page, with
  Older/Newer nav.

- Non-post pages (e.g. About, whatever) authored in
  Markdown. (Currently can plop any old HTML in WWW, but, (a) it won't
  use the master page of bodies->page and (b) it won't be auto-linked.

- Top navbar. Including non-post pages; see previous item.

- Share buttons (at least mailto:, Twitter and Google+).

- Get the responsive stuff working.

- sitemap.txt

DONE:

- Post pages: Add Older/Newer nav.

- Index pages: Sort posts in reverse date order.

- DRAFT tag to exclude from generation.

- Feeds for every tag.

|#
