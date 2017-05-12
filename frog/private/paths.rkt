#lang racket/base

(require net/uri-codec
         racket/contract/base
         racket/contract/region
         racket/match
         racket/path ;moved to racket/base only as of Racket 6
         racket/runtime-path
         racket/string
         rackjure/str
         rackjure/threading
         "params.rkt"
         "util.rkt")

(provide top
         example
         src-path
         src/posts-path
         post-template.html
         page-template.html
         index-template.html
         obj-path
         www-path
         www/tags-path
         www/feeds-path
         www/img-path
         www/index-pathname
         path->relative-path
         abs->rel/www
         abs->rel/src
         abs->rel/top
         canonical-uri
         full-uri
         permalink-path
         post-path->link
         slug
         editor-command-string)

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

(define/contract (www-path) (-> absolute-path?)
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
              (path->relative-path (current-posts-index-uri))))

(define (path->relative-path p) ;; path-string? -> path-string?
  (if (absolute-path? p)
      (apply build-path (cdr (explode-path p))) ;remove leading / \ or C:\
      p))

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

;; Convert an absolute local path to a URI path string relative to
;; `www-path` -- which in turn is relative to `current-output-dir`.
;; The result is always in Unix style (even on Windows) so it is
;; suitable for use as a URI path.
;;
;; Ex: if project top is /project/blog and the output dir is ../build,
;; then given "/project/build/css" this should return "/css". Same result
;; if on Windows and top is c:\project\blog and output dir is ..\build.
;;
;; NOTE: If you're creating a URI that a client will use to make an
;; HTTP request -- e.g. you will write it in an HTML, feed, or sitemap
;; file -- this result isn't sufficient. You should run the result
;; through `canonical-uri`, and if you need the scheme/host prepended,
;; finally through `full-uri`.
(define/contract (abs->rel/www path) (-> path? string?)
  (define segments (abs->rel 'abs->rel/www (simplify-path path) (www-path)))
  (string-append "/" (string-join (map path->string segments) "/")))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-output-dir "."])
                  (abs->rel/www (string->path "/projects/blog/css")))
                "/css")
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-output-dir "../build"])
                  (abs->rel/www (string->path "/projects/build/css")))
                "/css")
  (check-exn #rx"No common prefix: #<path:/not/projects/build/css> and #<path:/projects/build/>"
             (λ () (parameterize ([top "/projects/blog"]
                                  [current-output-dir "../build"])
                     (abs->rel/www (string->path "/not/projects/build/css"))))))

;; Convert an absolute local path to a local path-string relative to
;; (src-path).
(define/contract (abs->rel/src path) (-> path? path-string?)
  (define segments (abs->rel 'abs->rel/src (simplify-path path) (src-path)))
  (path->string (apply build-path segments)))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-source-dir "source"])
                  (abs->rel/src (string->path "/projects/blog/source/foo.md")))
                "foo.md")
  (check-equal? (parameterize ([top "/projects/blog"]
                               [current-source-dir "../blog-source"])
                  (abs->rel/src (string->path "/projects/blog-source/foo.md")))
                "foo.md"))

;; Convert an absolute local path to a local path-string relative to
;; (top) i.e. the project root dir.
(define/contract (abs->rel/top path) (-> path? path-string?)
  (define segments (abs->rel 'abs->rel/top (simplify-path path) (build-path* (top))))
  (path->string (apply build-path segments)))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"])
                  (abs->rel/top (string->path "/projects/blog/foo.md")))
                "foo.md")
  (check-equal? (parameterize ([top "/projects/blog"])
                  (abs->rel/top (string->path "/projects/blog/source/foo.md")))
                "source/foo.md"))

(define (abs->rel who a b) ;; (-> symbol? path? path? (listof path?)
  (define as (explode-path a))
  (define bs (explode-path b))
  (define-values (prefix tail _) (split-common-prefix as bs))
  (unless (equal? prefix bs)
    (error who "No common prefix: ~v and ~v" a b))
  tail)

;; Possibly rewrite a URI path to take account of non-#f
;; current-uri-prefix, and also uri-path-segment-encode it.
(define/contract (canonical-uri uri-path) (-> path-string? string?)
  (define (reroot p) ;; (-> string? path-string?)
    (if (and (current-uri-prefix) (absolute-path? p))
        (build-path (current-uri-prefix) (path->relative-path p))
        p))
  (define (encode p) ;; (-> path-string? string?)
    (define encode-seg (compose1 uri-path-segment-encode path->string))
    (define segs
      (if (absolute-path? p) ;don't encode the leading / or c:\
          (match (explode-path p) [(cons x xs) (list* x (map encode-seg xs))])
          (map encode-seg (explode-path p))))
    (path->string (apply build-path segs)))
  (encode (reroot uri-path)))

(module+ test
  (parameterize ([current-uri-prefix #f])
    (check-equal? (canonical-uri "r/λ/p") "r/%CE%BB/p")
    (check-equal? (canonical-uri "/a/λ/p") "/a/%CE%BB/p"))
  (parameterize ([current-uri-prefix "/prefix"])
    (check-equal? (canonical-uri "r/λ/p") "r/%CE%BB/p")
    (check-equal? (canonical-uri "/a/λ/p") "/prefix/a/%CE%BB/p"))
  (parameterize ([current-uri-prefix "/prefix///"])
    (check-equal? (canonical-uri "/a/λ/p") "/prefix/a/%CE%BB/p")))

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

(define (slug s)
  ;; Convert a string into a "slug", in which non-alpha and -numeric
  ;; chars are replaced by hyphens, and, the string is Unicode
  ;; normalized to NFD form.
  ;;
  ;; WARNING: Changing this will break blog post permalink patterns
  ;;that use the {title} variable. Even if this could be improved,
  ;;doing so would break backward compatability.
  (~>
   ;; First normalize string to Unicode composite form, so e.g. á will
   ;; be a single char for which char-alphabetic? is true. (In the
   ;; decomposed form á would be a plain a char followed by an accent
   ;; char, and the latter is not char-alphabetic? and would get
   ;; slugged to a hyphen.)
   (for/list ([c (in-string (string-normalize-nfc s))])
     (cond [(or (char-alphabetic? c)
                (char-numeric? c)) c]
           [else #\-]))
   list->string
   ;; Only one consecutive hyphen
   (regexp-replace* #px"-{2,}"  _ "-")
   ;; No trailing hyphen
   (regexp-replace  #px"-{1,}$" _ "")
   ;; Finally normalize to decomposed form. The rationale is that if
   ;; you use this result in a filename it will (hopefully) be
   ;; consistent across filesystems like Linux vs macOS.
   string-normalize-nfd))

(module+ test
  (require rackunit)
  (check-equal? (slug "?") "")
  ;; Sadly we don't trim leading hyphens, because we didn't from day
  ;; one therefore changing it now would break old URIs -- both link
  ;; URLs and feed URNs. So the following test is, alas, correct:
  (check-equal? (slug "'Foo, bar'") "-Foo-bar")
  (check-equal? (slug "Foo? Bar. Baz.")
                "Foo-Bar-Baz")
  (check-equal? (slug "Here's a question--how many hyphens???")
                "Here-s-a-question-how-many-hyphens"))

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


