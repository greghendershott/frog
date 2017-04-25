#lang rackjure/base

(require racket/contract/base
         racket/contract/region
         racket/match
         racket/path ;moved to racket/base only as of Racket 6
         racket/runtime-path
         rackjure/str
         rackjure/threading
         "params.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit
           racket/function)
  ;; For testing only, define some root directory
  (define root (if (eq? 'windows (system-path-convention-type)) "C:\\" "/")))

;; top is the project directory (e.g. the main dir in Git)
(define top (make-parameter #f))

;; For interactive development and for --init feature.
(define-runtime-path example "../../example/")

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
  (check-equal? (parameterize ([top (build-path root "projects" "blog")]
                               [current-source-dir "_src"])
                  (src-path))
                (path->directory-path
                 (build-path root "projects" "blog" "_src")))
  (check-equal? (parameterize ([top (build-path root "projects" "blog")]
                               [current-source-dir (build-path 'up "source")])
                  (src-path))
                (path->directory-path
                 (build-path root "projects" "source"))))

;; some specific source files
(define (post-template.html)
  (build-path (src-path) "post-template.html"))

(define (page-template.html)
  (build-path (src-path) "page-template.html"))

(define (index-template.html)
  (build-path (src-path) "index-template.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intermediate file directory

(define (obj-path)
  (build-path (top) ".frog"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output directories

(define (www-path)
  (define out (current-output-dir))
  (cond [(relative-path? out) (build-path* (top) out)]
        [else                 (build-path*       out)]))

(module+ test
  (check-equal? (parameterize ([top (build-path root "projects" "blog")]
                               [current-output-dir (build-path 'same)])
                  (www-path))
                (path->directory-path
                 (build-path root "projects" "blog")))
  (check-equal? (parameterize ([top (build-path root "projects" "blog")]
                               [current-output-dir (build-path 'up "build" "stuff")])
                  (www-path))
                (path->directory-path
                 (build-path root "projects" "build" "stuff"))))

(define (www/tags-path) (build-path* (www-path) "tags"))
(define (www/feeds-path) (build-path* (www-path) "feeds"))
(define (www/img-path) (build-path* (www-path) "img"))

(define (www/index-pathname)
  ;; Handle current-post-index-uri being any of /path/index.html,
  ;; \path\index.html, or c:\path\index.html
  (build-path (www-path)
              (~> (current-posts-index-uri)
                  string->path
                  path->relative-path)))

(define (path->relative-path p) ;; path? -> path?
  (cond [(relative-path? p) p]
        [else (apply build-path
                     ;; remove leading / \ or C:\
                     (~> p explode-path cdr))]))

(module+ test
  (parameterize ([top (build-path root "projects" "blog")]
                 [current-output-dir (build-path 'up "build" "stuff")])
    ;; absolute
    (check-equal? (parameterize ([current-posts-index-uri "/index.html"])
                    (www/index-pathname))
                  (build-path root "projects" "build" "stuff"
                              "index.html"))
    (check-equal? (parameterize ([current-posts-index-uri "/foo/bar.html"])
                    (www/index-pathname))
                  (build-path root "projects" "build" "stuff"
                              "foo" "bar.html"))
    ;; relative
    (check-equal? (parameterize ([current-posts-index-uri "index.html"])
                    (www/index-pathname))
                  (build-path root "projects" "build" "stuff"
                              "index.html"))
    (check-equal? (parameterize ([current-posts-index-uri "foo/bar.html"])
                    (www/index-pathname))
                  (build-path root "projects" "build" "stuff"
                              "foo" "bar.html"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert an absolute local path to a path string relative to
;; (www-path), and always in Unix style (even on Windows) so it is
;; suitable for use as a URI path.
;;
;; Ex: if project top is /project/blog and the output dir is ../build,
;; then given "/project/build/css" this should return "/css". Same result
;; if on Windows and top is c:\project\blog and output dir is ..\build.
(define (abs->rel/www path) ;; path? -> string?
  (let ([path (~> path simplify-path path->string)]
        [root (~> (www-path) path->string)])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+)$") (list _ s))
       (str "/" (regexp-replaces s '([#rx"\\\\" "/"])))]
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

;; Convert an absolute local path to a path string relative to
;; (src-path).
(define (abs->rel/src path) ;; path? -> string?
  (let ([path (~> path simplify-path path->string)]
        [root (~> (src-path) path->string)])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+)$") (list _ x)) x]
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

;; Convert an absolute local path to a path string relative to
;; (top) i.e. the project root dir.
(define (abs->rel/top path) ;; path? -> string?
  (let ([path (~> path simplify-path path->string)]
        [root (~> (top) build-path* path->string)])
    (match path
      [(pregexp (str "^" (regexp-quote root) "(.+)$") (list _ x)) x]
      [_ (raise-user-error 'abs->rel/top "root: ~v path: ~v" root path)])))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"])
                  (abs->rel/top (string->path "/projects/blog/foo.md")))
                "foo.md")
  (check-equal? (parameterize ([top "/projects/blog"])
                  (abs->rel/top (string->path "/projects/blog/source/foo.md")))
                "source/foo.md"))

;; Possibly rewrite a URI path to take account of the current root
;; If full is given make it a full URI, or explode if it makes no sense
;; This probably should handle URIs with protocol & host, but does not.
(define/contract (canonicalize-uri uri-path)
  (string? . -> . string?)
  (match* ((current-uri-prefix) uri-path)
    [((? values prefix) (pregexp "^/.*")) (str prefix uri-path)]
    [(_                 uri-path        ) uri-path]))

(module+ test
  (let ([au "/absolute/path"]
        [ru "relative/path"])
    (parameterize ([current-uri-prefix #f])
      (check-equal? ru (canonicalize-uri ru))
      (check-equal? au (canonicalize-uri au)))
    (parameterize ([current-uri-prefix "/reroot"])
      (check-equal? ru (canonicalize-uri ru))
      (check-equal? (str (current-uri-prefix) au)
                    (canonicalize-uri au)))
    (parameterize ([current-uri-prefix "/reroot///"])
      (check-equal? (str "/reroot" au) (canonicalize-uri au)))))

;; Given a uri-path, prepend the scheme & host to make a full URI.
(define (full-uri uri-path)
  (match uri-path
    [(pregexp #px"^/")
     (str (current-scheme/host) uri-path)]
    [_ (raise-user-error 'full-uri
                         "can't attach host/scheme to relative path")]))

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

;; Given a path, return a URI path. Also, if the path ends in
;; "/index.html", return the path without the "index.html" suffix.
(define/contract (post-path->link pp)
  (path? . -> . string?)
  (match (abs->rel/www pp) ;assumes abs->rel/www always returns 'unix style
    [(pregexp "^(.+?)/index.html" (list _ s)) (str s "/")]
    [s s]))

(module+ test
  (parameterize ([top (find-system-path 'home-dir)])
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post.html"))
     "/blog/2012/05/31/title-of-post.html")
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post/index.html"))
     "/blog/2012/05/31/title-of-post/")))

;; I'm not really sure where to put this so ...
(define/contract (editor-command-string editor filename pattern)
  (string? string? string? . -> . string?)
  (regexp-replaces pattern
                   `([#rx"{editor}" ,editor]
                     [#rx"{filename}",filename])))

(module+ test
  (parameterize ([top (find-system-path 'home-dir)])
    (define f (curry editor-command-string
                     "vim" "2012-05-31-title-of-post.md"))
    (check-equal? (f "{editor} {filename}")
                  "vim 2012-05-31-title-of-post.md")
    (check-equal? (f "emacsclient /tmp/draft.md")
                  "emacsclient /tmp/draft.md")
    (check-equal? (f "exec {editor} {filename} >/dev/null 2>&1 &")
                  "exec vim 2012-05-31-title-of-post.md >/dev/null 2>&1 &")
    (check-equal? (f "exo-open --launch TerminalEmulator '{editor} {filename}'")
                  "exo-open --launch TerminalEmulator 'vim 2012-05-31-title-of-post.md'")))


