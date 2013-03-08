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

(struct index (title uri date tags blurb more?))

;; A function for fold-files
(define (write-post path type v)
  (cond
    [(eq? type 'file)
     (define-values (base name must-be-dir?) (split-path path))
     (match (path->string name)
       [(pregexp "^\\d{4}-\\d{2}-\\d{2}-.+?\\.(?:md|markdown)$")
        (define dest-path (~> (build-path (www-path) name)
                              (path-replace-suffix ".html")))
        (define xs (with-input-from-file path read-markdown))
        ;; Split to the meta-data and the body
        (define-values (title date tags body) (meta-data xs))
        ;; Add these tags to the set
        (for ([x tags])
          (unless (equal? x "")
            (hash-set! all-tags x (add1 (hash-ref all-tags x 0)))))
        ;; Split out the blurb (may be less than the entire body)
        (define-values (blurb more?) (above-the-fold body))
        ;; Make up a body for the post
        (define post-body (post-xexpr title date tags body))
        (~> post-body
            (bodies->page title)
            xexpr->string
            (display-to-file dest-path #:exists 'replace))
        (eprintf (str "Generated ~a\n"
                      "     from ~a\n") dest-path path)
        (cons (index title
                     (abs->rel dest-path)
                     date
                     tags
                     blurb
                     more?)
              v)]
       [else
        (eprintf "Skipping ~s; not named YYYY-MM-DD-title.{md,markdown}\n"
                 path)
        v])]
    [else v]))

;; A function for fold-files
(define (do-feed-item path type v)
  (cond
    [(eq? type 'file)
     (define-values (base name must-be-dir?) (split-path path))
     (match (path->string name)
       [(pregexp "^\\d{4}-\\d{2}-\\d{2}-.+?\\.(?:md|markdown)$")
        (define xs (with-input-from-file path read-markdown))
        ;; Split to the meta-data and the body
        (define-values (title date tags body) (meta-data xs))
        ;; Add these tags to the set
        (for ([x tags])
          (unless (equal? x "")
            (hash-set! all-tags x (add1 (hash-ref all-tags x 0)))))
        ;; Split out the blurb (may be less than the entire body)
        (define-values (blurb more?) (above-the-fold body))

        (define uri (path->string path)) ;; TODO: real URI, with hostname
        (cons
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
                        [else '()])))))
         v)]
       [else
        (eprintf "Skipping ~s; not named YYYY-MM-DD-title.{md,markdown}\n"
                 path)
        v])]
    [else v]))

(define (post-xexpr title date tags body)
  (list* `(h1 ,title)
         `(p ,date)
          (tags->xexpr tags)
          (filter (negate more?) body)))
         
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

(define (abs->rel path)
  (let ([path (path->string path)]
        [root (path->string (www-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is really a "master page" or "site template". It puts the body
;; elements in a container div.
(define-runtime-path
  google-analytics-template.js
  "google-analytics-template.js")
(define (bodies->page xs title)
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
    `(p "Posts tagged:"
        (ul ,@(for/list ([(k v) (in-dict alist)])
                `(li ,(tag->xexpr k (format " (~a)" v)))))))
  `(html
    ([lang "en"])
    (head
     (title ,title)
     (meta ([name "viewport"]
            [content "width=device-width, initial-scale=1.0"]))
     (meta ([name "author"][content ,(current-author)]))
     (link ([href "/css/bootstrap.css"][rel "stylesheet"][type "text/css"]))
     (link ([href "/css/custom.css"][rel "stylesheet"][type "text/css"]))
     (link ([href "/atom.xml"]
            [type "application/atom+xml"]
            [rel "alternate"]
            [title ,(current-title)]))
     (script ([type "text/javascript"])
             ,(format (file->string google-analytics-template.js)
                      (current-google-analytics-account)
                      (current-google-analytics-domain))))
    (body

     (div ([class "navbar navbar-fixed-top"])
          (div ([class "navbar-inner"])
               (div ([class "container"])
                    (p ([class "pull-left"])
                       (img ([style "width: 42px; height:33px;"]
                             [src "/img/gh-head-bw.jpg"]) 'nbsp))
                    (a ([class "brand"]
                        [href="#"])
                       ,(current-title)))))

     (div ([class "container"])
          (div ([class "row"])
               ;; Docs sidebar
               (div ([class "span2 bs-docs-sidebar"])
                    ,(tag-cloud-xexpr)
                    ,(toc-xexpr))
               ;; Main content
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

(define (write-index xs title file) ;; (listof index?) -> any
  (define (string-prepend s prepend)
    (string-append prepend s))
  (~> (for/list ([x (in-list xs)])
        (match-define (index title uri date tags blurb more?) x)
        `(div ([class "index-post"])
              (h1 (a ([href ,uri]) ,title))
              (p ,date)
              ,(tags->xexpr tags)
              ,@blurb
              ,@(cond [more? `((a ([href ,uri]) (em "Continue reading ...")))]
                      [else '()])))
      (add-between `(hr))
      (bodies->page title)
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

(define (write-atom-feed)
  (define x
    `(feed
      ([xmlns "http://www.w3.org/2005/Atom"]
       [xml:lang "en"])
      (title ([type "text"]) ,(current-title))
      ;; (link ([href ,(gplus-self-uri g)]
      ;;        [rel "self"]))
      ;; (link ([href ,(gplus-self-uri g)]))
      ;; (id () ,(gplus-id g))
      ;; (etag () ,(gplus-etag g))
      ;; (updated () ,(gplus-updated g))
      ,@(fold-files do-feed-item '() (src/posts-path) #f)))
  (~> x
      xexpr->string
      (display-to-file (build-path (www-path) "atom.xml")
                       #:exists 'replace)))

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
  (define (two-digits d)
    (cond [(< d 10) (str "0" (~a d))]
          [else (~a d)]))
  (define d (current-date))
  (define date-str (str #:sep "-"
                        (~> d date-year two-digits)
                        (~> d date-month two-digits)
                        (~> d date-day two-digits)))
  (define filename (str date-str
                        "-"
                        (~> title
                            string-downcase
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
      (eprintf "Deleted ~a\n" p))
    (cond
     [(eq? type 'file)
      (define-values (base name must-be-dir?) (split-path path))
      (cond [(equal? path (build-path (www-path) "index.html"))
             (rm path)]
            [(equal? (build-path base) (build-path (www-path) "tags/"))
             (rm path)]
            [else (match (path->string name)
                    [(pregexp "^\\d{4}-\\d{2}-\\d{2}-.+?\\.html$")
                     (rm path)]
                    [else (void)])])]))
  (fold-files maybe-delete '() (www-path) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build)
  (define (post<=? a b)
    (string<=? (index-date a) (index-date b)))

  ;; Write the posts and get (listof index?) with which to do more work.
  (define xs (~> (fold-files write-post '() (src/posts-path) #f)
                 (sort (negate post<=?))))
  ;; Write the index page for each tag
  (for ([(tag _) (in-hash all-tags)])
    (write-index (filter (lambda (x)
                           (member tag (index-tags x)))
                         xs)
                 tag
                 (build-path (www-path) "tags" (str (our-encode tag) ".html"))))
  ;; Write the index for all
  (write-index xs "All Tags" (build-path (www-path) "index.html"))
  ;; Write Atom feed file
  (write-atom-feed))

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

;; (clean)
;; (build)
;; (preview)

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

- Index pages: Sort posts in reverse date order.

- Index pages: Paingate, limiting to N posts per page, with
  Older/Newer nav.

- Post pages: Add Older/Newer nav.

- DRAFT tag to exclude from generation.

|#
