#lang rackjure

(require racket/runtime-path
         "params.rkt")

(provide (all-defined-out))

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
(define (www/img-path) (build-path (www-path) "img"))

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

;; Given a uri-path, prepend the scheme & host to make a full URI.
(define (full-uri uri-path)
  (str (current-scheme/host) uri-path))

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
  (parameterize ([top (find-system-path 'home-dir)])
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
  (parameterize ([top (find-system-path 'home-dir)])
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post.html"))
     "/blog/2012/05/31/title-of-post.html")
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post/index.html"))
     "/blog/2012/05/31/title-of-post/")))
