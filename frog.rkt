#lang rackjure

(require racket/runtime-path
         markdown
         xml
         racket/date
         (only-in srfi/1 break))

;; top is the source directory
(define-runtime-path example "example")
(define top (make-parameter example))

;; source posts
(define (posts-path) (build-path (top) "posts"))

;; destinations, from root of the generated web site on down
(define (www-path) (build-path (top) "www"))
(define (www/tags-path) (build-path (top) "www" "tags"))

(define all-tags (make-hash)) ;; (hashof string? exact-positive-integer?)

(struct index (title uri date tags blurb more?))

(define (do-post path type v)
  (cond
    [(eq? type 'file)
     (define-values (base name must-be-dir?) (split-path path))
     (match (path->string name)
       [(pregexp "^(\\d{4})-(\\d{2})-(\\d{2})-.+?\\.(?:md|markdown)$"
                 (list _ yr mo dy))
        (define dest-path (~> (build-path (www-path) name)
                              (path-replace-suffix ".html")))
        ;;(eprintf "~a =>\n~a\n" path dest-path)
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
    [else (eprintf "Skipping ~s: not a file\n" path) v]))

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

;; This is really a "master page" or "site template". It puts the body
;; elements in a container div.
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
                      (sort <= #:key cdr)))
    `(p "Posts tagged:"
        (ul ,@(for/list ([(k v) (in-dict alist)])
                `(li ,(tag->xexpr k (format " (~a)" v)))))))
  `(html
    ([lang "en"])
    (head
     (title ,title)
     (meta ([name "viewport"]
            [content "width=device-width, initial-scale=1.0"]))
     (meta ([name "author"][content "Greg Hendershott"]))
     (link ([href "/css/bootstrap.css"]
            [rel "stylesheet"]
            [type "text/css"]))
     (style ([type "text/css"])
      "body {"
      "  padding-top: 60px;"
      "  padding-bottom: 40px;"
      "}"
      ".sidebar-nav {"
      "  padding: 9px 0;"
      "}"))
    (body
     ([data-spy "scroll"][data-target ".bs-docs-sidebar"])

     (div ([class "navbar navbar-fixed-top"])
          (div ([class "navbar-inner"])
               (div ([class "container"])
                    (p ([class "pull-left"])
                       (img ([style "width: 42px; height:33px;"]
                             [src "/img/gh-head-bw.jpg"]) 'nbsp))
                    (a ([class "brand"]
                        [href="#"])
                       "Greg Hendershott"))))

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
           (p "Markdown edited with "
              (a ([href "http://www.gnu.org/software/emacs/"])
                 "GNU Emacs") ".")
           (p "Generated by Frog, the frozen blog tool.")
           (p "Using " (a ([href "http://twitter.github.com/bootstrap/index.html"])
                          "Bootstrap") ".")
           (p "Names may be trademarks, yada yada.")
           (p "Copyright " 'copy
              "2012-2013 by Greg Hendershott. All rights reserved.")))
     )))

(define (abs->rel path)
  (let ([path (path->string path)]
        [root (path->string (www-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x])))

(define (write-index xs title file) ;; (listof index?) -> any
  (define (string-prepend s prepend)
    (string-append prepend s))
  (define (date<=? a b)
    (string<=? (index-date a) (index-date b)))
  (~> (for/list ([x (sort xs date<=?)])
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
  (define pathname (build-path (posts-path) filename))
  (when (file-exists? pathname)
    (raise-user-error "File ~a already exists! Did NOT overwrite" pathname))
  (display-to-file (format new-post-template
                           title date-str)
                   pathname
                   #:exists 'error)
  (eprintf "Created ~a\n" pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clean)
  (define (maybe-delete path type v)
    (define (rm p)
      (delete-file p)
      (eprintf "Deleted ~a\n" p))
    (cond
     [(eq? type 'file)
      (define-values (base name must-be-dir?) (split-path path))
      ;; (displayln (build-path base))
      ;; (displayln (build-path (www-path) "tags/"))
      (cond [(equal? path (build-path (www-path) "index.html"))
             (rm path)]
            [(equal? (build-path base) (build-path (www-path) "tags/"))
             (rm path)]
            [else (match (path->string name)
                    [(pregexp "^\\d{4}-\\d{2}-\\d{2}-.+?\\.html$")
                     (rm path)]
                    [else (void)])])])
    (void))
  (fold-files maybe-delete '() (www-path) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rebuild)
  ;; Write the posts
  (define xs (fold-files do-post '() (posts-path) #t))
  ;; Write the index page for each tag
  (for ([(k _) (in-hash all-tags)])
    (write-index (filter (lambda (x)
                           (member k (index-tags x)))
                         xs)
                 k
                 (build-path (www-path) "tags" (str (our-encode k) ".html"))))
  ;; Write the index for all
  (write-index xs "All Tags" (build-path (www-path) "index.html")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in serve: lobe/serve-files)
         net/sendurl)

(define (preview)
  ;; Fire up a local server to preview
  (define stop
    (parameterize ([serve:home (www-path)]
                   [serve:port 3000]
                   [serve:dotfiles? #f])
      (serve:start)))
  (send-url "http://localhost:3000/index.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clean)
(rebuild)
(preview)
