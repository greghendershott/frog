#lang racket/base

(require racket/require
         net/uri-codec
         (multi-in racket (contract format file match string))
         threading
         (only-in markdown xexpr->string)
         "bodies-page.rkt"
         "feeds.rkt"
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "template.rkt"
         (only-in "util.rkt"
                  delete-file*
                  delete-files*
                  display-to-file*
                  in-slice)
         "verbosity.rkt")

(provide clean-tag-output-files
         write-stuff-for-tag
         index-path-for-tag
         atom-path-for-tag
         rss-path-for-tag)

(module+ test
  (require rackunit))

;; Each tag has index page(s) and feed files.
;;
;; Includes the special tag "all", meaning all posts.

(define (clean-tag-output-files)
  (delete-files* (www/tags-path) abs->rel/www)
  (delete-files* (www/feeds-path) abs->rel/www))

(define/contract (write-stuff-for-tag tag posts)
  (string? (listof post?) . -> . void)
  (define title (title-for-tag tag))
  (write-index-pages posts
                     title
                     (if (equal? tag "all") #f tag)
                     (slug tag)
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
    [_     (build-path (www/tags-path) (~a (slug tag) ".html"))]))

(define (index-uri-for-tag tag)
  (match tag
    ["all" (canonical-uri (current-posts-index-uri))]
    [_     (canonical-uri (abs->rel/www (index-path-for-tag tag)))]))

(define (title-for-tag tag)
  (match tag
    ["all" (current-title)]
    [_     (~a "Posts tagged '" tag "'")]))

(define (atom-path-for-tag tag)
  (build-path (www/feeds-path) (~a (slug tag) ".atom.xml")))

(define (rss-path-for-tag tag)
  (build-path (www/feeds-path) (~a (slug tag) ".rss.xml")))

(define (write-index-pages xs    ;(listof post?) -> any
                           title ;string?
                           tag   ;(or/c #f string?)
                           feed  ;string?
                           file) ;path?
  (define num-posts (length xs))
  (define num-pages (ceiling (/ num-posts (current-posts-per-page))))
  (for ([page-num (in-range num-pages)]
        [page-posts (in-slice (current-posts-per-page) xs)])
    (write-index-page page-posts
                      (cond [(zero? page-num) title]
                            [else (~a title " (page " (add1 page-num) ")")])
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
        (define content-only (cond [(current-index-full?) body]
                                   [else blurb]))
        (define effectively-more? (and more? (not (current-index-full?))))
        (define content (string-append
                         content-only
                         (cond [effectively-more?
                                (xexpr->string
                                 `(footer ([class "read-more"])
                                   (a ([href ,uri-path]) hellip "more" hellip)))]
                               [else ""])))
        (define tpl "index-template.html")
        (ensure-index-template tpl)
        (render-template
         (src-path)
         tpl
         (hasheq 'title        (title->htmlstr title)
                 'uri-prefix   (or (current-uri-prefix) "")
                 'uri-path     uri-path
                 'full-uri     (full-uri uri-path)
                 'date-8601    date
                 'date-struct  (date->date-struct date)
                 'date         (~> date date->xexpr xexpr->string)
                 'tags         (~> tags tags->xexpr xexpr->string)
                 'authors      (~> tags author-tags->xexpr xexpr->string)
                 'date+tags    (~> (date+tags->xexpr date tags) xexpr->string)
                 'content      content
                 'content-only content-only
                 'more?        effectively-more?)))
      (string-join "\n")
      (string-append
       (if (> num-pages 1)
           (xexpr->string `(footer ,(bootstrap-pagination base-file page-num num-pages))) ""))
      (bodies->page #:title title
                    #:description title
                    #:feed feed
                    #:uri-path (canonical-uri (abs->rel/www file))
                    #:keywords (cond [tag (list tag)]
                                     [else (hash-keys (all-tags))])
                    #:tag tag)
      (display-to-file* file #:exists 'replace)))

(define (ensure-index-template tpl)
  ;; For users of old versions of Frog: If project has no
  ;; index-template.html, copy the one from example. Much like
  ;; --init does, but just this one file.
  (define to (~> (build-path (src-path) tpl) simplify-path))
  (unless (file-exists? to)
    (define from (~> (build-path example "_src" tpl) simplify-path))
    (prn0 "~a does not exist. Copying from ~a" to from)
    (copy-file from to)))

(define (bootstrap-pagination base-file page-num num-pages)
  `(ul ([class "pagination"])
       ,(cond [(zero? page-num) `(li ([class "page-item disabled"])
                                     (a ([class "page-link"]
                                         [href "#"]) 'larr))]
              [else `(li ([class "page-item"])
                         (a ([class "page-link"]
                             [href ,(~> (file/page base-file (sub1 page-num))
                                        abs->rel/www canonical-uri)])
                            'larr))])
       ,@(for/list ([n (in-range num-pages)])
           `(li (,@(cond [(= n page-num) `([class "page-item active"])]
                         [else '([class "page-item"])]))
                (a ([class "page-link"]
                    [href ,(~> (file/page base-file n) abs->rel/www
                               canonical-uri)])
                   ,(number->string (add1 n)))))
       ,(cond [(= (add1 page-num) num-pages)
               `(li ([class "page-item disabled"])
                    (a ([class "page-link"] [href "#"]) 'rarr))]
              [else `(li ([class "page-item"])
                         (a ([class "page-link"]
                             [href ,(~> (file/page base-file (add1 page-num))
                                        abs->rel/www canonical-uri)])
                            'rarr))]) ))

(define (file/page base-file page-num)
  (cond [(zero? page-num) base-file]
        [else (~> base-file             ;add "-<page>" suffix
                  (path-replace-suffix "")
                  path->string
                  (~a "-" (add1 page-num))
                  string->path
                  (path-replace-suffix ".html"))]))
