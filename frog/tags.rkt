#lang rackjure

(require (only-in markdown xexpr->string)
         "bodies-page.rkt"
         "feeds.rkt"
         "paths.rkt"
         "params.rkt"
         "post-struct.rkt"
         "take.rkt"
         "util.rkt"
         "template.rkt"
         "verbosity.rkt"
         )

(provide (all-defined-out))

;; Each tag has index page(s) and feed files.
;;
;; Includes the special tag "all", meaning all posts.

(define (clean-tag-output-files)
  (define (maybe-delete path type v)
    (when (eq? type 'file)
      (delete-file* path abs->rel/www)))
  (fold-files maybe-delete '() (build-path (www-path) "tags/") #f)
  (fold-files maybe-delete '() (build-path (www-path) "feeds/") #f))

(define/contract (write-stuff-for-tag tag posts)
  (string? (listof post?) . -> . void)
  (define title (title-for-tag tag))
  (write-index-pages posts
                     title
                     (if (equal? tag "all") #f tag)
                     (our-encode tag)
                     (index-path-for-tag tag))
  (define index-uri (index-uri-for-tag tag))
  (write-atom-feed posts
                   title
                   tag
                   index-uri
                   (atom-path-for-tag tag))
  (write-rss-feed posts
                  title
                  tag
                  index-uri
                  (rss-path-for-tag tag)))

(define (index-path-for-tag tag)
  (match tag
    ["all" (www/index-pathname)]
    [_     (build-path (www/tags-path) (str (our-encode tag) ".html"))]))

(define (index-uri-for-tag tag)
  (match tag
    ["all" (current-posts-index-uri)]
    [_     (abs->rel/www (index-path-for-tag tag))]))

(define (title-for-tag tag)
  (match tag
    ["all" (current-title)]
    [_     (str "Posts tagged '" tag "'")]))

(define (atom-path-for-tag tag)
  (build-path (www/feeds-path) (str (our-encode tag) ".atom.xml")))

(define (rss-path-for-tag tag)
  (build-path (www/feeds-path) (str (our-encode tag) ".rss.xml")))

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
                      (cond [(zero? page-num) title]
                            [else (str title " (page " (add1 page-num) ")")])
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
  (prn1 "Generating ~a" (abs->rel/www file))
  (~> (for/list ([x (in-list xs)])
        (match-define
         (post title src modtime dest-path uri-path date older newer tags blurb more? body) x)
        (define content
          (cond [(current-index-full?) body]
                [more? (string-append
                        blurb
                        (xexpr->string
                         `(footer ([class "read-more"])
                           (a ([href ,uri-path]) hellip "more" hellip))))]
                [else blurb]))
        ;; For users of old versions of Frog: If project has no
        ;; index-template.html, copy the one from example. Much like
        ;; --init does, but just this one file.
        (define tpl "index-template.html")
        (define to (~> (build-path (src-path) tpl) simplify-path))
        (unless (file-exists? to)
          (define from (~> (build-path example "_src" tpl) simplify-path))
          (prn0 "~a does not exist. Copying from ~a" to from)
          (copy-file from to))
        (render-template
         (src-path)
         tpl
         {'title (title->htmlstr title)
          'uri-path uri-path
          'full-uri (full-uri uri-path)
          'date-8601 date
          'date-struct (date->date-struct date)
          'date (~> date date->xexpr xexpr->string)
          'tags (~> tags tags->xexpr xexpr->string)
          'date+tags (~> (date+tags->xexpr date tags) xexpr->string)
          'content content}))
      (string-join "\n")
      (string-append (xexpr->string `(footer ,(bootstrap-pagination base-file page-num num-pages))))
      (bodies->page #:title title
                    #:description title
                    #:feed feed
                    #:uri-path (abs->rel/www file)
                    #:keywords (cond [tag (list tag)]
                                     [else (hash-keys (all-tags))])
                    #:tag tag)
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
