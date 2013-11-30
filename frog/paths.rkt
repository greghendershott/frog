#lang rackjure

(require racket/runtime-path
         "params.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; top is the project directory (e.g. the main dir in Git)
(define top (make-parameter #f))

;; For interactive development and for --init feature.
(define-runtime-path example "../example/")

;; Composition of build-path, expand-user-path, simplify-path, and
;; path->directory-path.
(define (build-path* . xs)
  (~> (apply build-path xs)
      expand-user-path                ;expand things like ~
      simplify-path                   ;factor out things like . and ..
      path->directory-path))          ;ensure trailing slash

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source directories

(define (src-path)
  (define src (current-source-dir))
  (cond [(relative-path? src) (build-path* (top) src)]
        [else                 (build-path*       src)]))

(define (src/posts-path) (build-path* (src-path) "posts"))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-source-dir "_src"])
                  (src-path))
                (path->directory-path
                 (build-path "/" "projects" "blog" "_src")))
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-source-dir "../source"])
                  (src-path))
                (path->directory-path
                 (build-path "/" "projects" "source"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output directories

(define (www-path)
  (define out (current-output-dir))
  (cond [(relative-path? out) (build-path* (top) out)]
        [else                 (build-path*       out)]))

(define (www/tags-path) (build-path* (www-path) "tags"))
(define (www/feeds-path) (build-path* (www-path) "feeds"))
(define (www/img-path) (build-path* (www-path) "img"))

(module+ test
 (check-equal? (parameterize ([top "/projects/blog"]
                              [current-output-dir "."])
                 (www-path))
               (path->directory-path
                (build-path "/" "projects" "blog")))
 (check-equal? (parameterize ([top "/projects/blog"]
                              [current-output-dir "../build/stuff"])
                 (www-path))
               (path->directory-path
                (build-path "/" "projects" "build" "stuff"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert an absolute local path to a path string relative to
;; (www-path). For example,what the URI path should be.
;;
;; Ex: if project top is /project/blog and the output dir is ../build,
;; then given "/project/build/css" this should return "/css".
(define (abs->rel/www path) ;; path? -> string?
  (let ([path (path->string path)]
        [root (path->string (www-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) (str "/" x)]
      [_ (raise-user-error 'abs->rel/www "root: ~v path: ~v" root path)])))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-output-dir "."])
                  (abs->rel/www (string->path "/projects/blog/css")))
                "/css")
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-output-dir "../build"])
                  (abs->rel/www (string->path "/projects/build/css")))
                "/css"))

;; Convert an absolute local path to a path string realtive to
;; (src-path).
(define (abs->rel/src path) ;; path? -> string?
  (let ([path (path->string path)]
        [root (path->string (src-path))])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+$)") (list _ x)) x]
      [_ (raise-user-error 'abs->rel/src "root: ~v path: ~v" root path)])))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-source-dir "source"])
                  (abs->rel/src (string->path "/projects/blog/source/foo.md")))
                "foo.md")
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-source-dir "../blog-source"])
                  (abs->rel/src (string->path "/projects/blog-source/foo.md")))
                "foo.md"))

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
