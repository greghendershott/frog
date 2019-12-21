#lang at-exp racket/base

(require racket/require
         net/uri-codec
         (multi-in racket/contract (base region))
         (multi-in racket (match string))
         threading
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual)
         "params.rkt"
         (multi-in "private" ("define-doc.rkt" "util.rkt")))

(module+ test
  (require rackunit
           racket/function)
  ;; For testing, define some root directory
  (define root (if (eq? 'windows (system-path-convention-type)) "C:\\" "/")))

(define top (make-parameter #f))
(provide
 (parameter-doc top
                (parameter/c (or/c #f absolute-path?))
                path
                @{The project directory root. Frog sets the value
                  after it has initialized and found the location of
                  @secref["config"]. Many other functions in this
                  module expect @racket[top] to be non-@racket[#f],
                  so for example in unit tests you may need to
                  set this yourself.}))

;; Composition of build-path, expand-user-path, simplify-path, and
;; path->directory-path.
(define (build-path* . xs)
  (~> (apply build-path xs)
      expand-user-path                ;expand things like ~
      simplify-path                   ;factor out things like . and ..
      path->directory-path))          ;ensure trailing slash


;;; Source directories

(define/doc (src-path absolute-path?)
  @{Resolved location of @racket[current-source-dir].}
  (define src (current-source-dir))
  (cond [(relative-path? src) (build-path* (top) src)]
        [else                 (build-path*       src)]))

(define/doc (src/posts-path absolute-path?)
  @{The @filepath{posts} subdirectory of @racket[src-path].}
  (build-path* (src-path) "posts"))

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
(define/doc (post-template.html absolute-path?)
  @{The @filepath{post-template.html} file in @racket[src-path].}
  (build-path (src-path) "post-template.html"))

(define/doc (page-template.html absolute-path?)
  @{The @filepath{page-template.html} file in @racket[src-path].}
  (build-path (src-path) "page-template.html"))

(define/doc (index-template.html absolute-path?)
  @{The @filepath{index-template.html} in @racket[src-path].}
  (build-path (src-path) "index-template.html"))


;;; Intermediate file directory

(define/doc (obj-path absolute-path?)
  @{The @filepath{.frog} build cache subdirectory.}
  (build-path (top) ".frog"))


;;; Output directories

(define/doc (www-path absolute-path?)
  @{Root of the output files, as determined by @racket[current-output-dir].}
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

(define/doc (www/tags-path absolute-path?)
  @{The @filepath{tags/} subdirectory of @racket[www-path].}
  (build-path* (www-path) "tags"))
(define/doc (www/feeds-path absolute-path?)
  @{The @filepath{feeds/} subdirectory of @racket[www-path].}
  (build-path* (www-path) "feeds"))
(define/doc (www/img-path absolute-path?)
  @{The @filepath{img/} subdirectory of @racket[www-path].}
  (build-path* (www-path) "img"))

(define/doc (www/index-pathname absolute-path?)
  @{Resolves @racket[current-posts-index-uri] regardless of it being any
    of @filepath{/path/index.html}, @filepath{\path\index.html}, or
    @filepath{c:\path\index.html}}
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

;;; abs->rel/*

(define/doc (abs->rel/www [path absolute-path?] string?)
  @{Convert an absolute local path to a URI path string relative to
    @racket[www-path] --- which in turn is relative to
    @racket[current-output-dir]. The result is always in Unix style
    (even on Windows) so it is suitable for use as a URI path.

    For example if @racket[top] is @filepath{/project/blog} and
    @racket[current-output-dir] is @filepath{../build},
    then given @filepath{/project/build/css} this should return
    @filepath{/css}. Same result if on Windows and @racket[top] is
    @filepath{c:\project\blog} and @racket[current-output-dir] is
    @filepath{..\build}.

    NOTE: If you're creating a URI that a client will use to make an
    HTTP request --- e.g. you will write it in an HTML, feed, or sitemap
    file --- this result isn't sufficient. You should run the result
    through @racket[canonical-uri], and if you need @racket[current-scheme/host]
    prepended, in turn through @racket[full-uri].}
  #:ex [(require frog/paths frog/params)]
  #:ex [(parameterize ([top "/projects/blog"]
                       [current-output-dir "."])
          (abs->rel/www (string->path "/projects/blog/css")))
        "/css"]
  #:ex [(parameterize ([top "/projects/blog"]
                       [current-output-dir "../build"])
          (abs->rel/www (string->path "/projects/build/css")))
        "/css"]
  (define segments (abs->rel 'abs->rel/www (simplify-path path) (www-path)))
  (string-append "/" (string-join (map path->string segments) "/")))

(module+ test
  (check-exn #rx"No common prefix: #<path:/not/projects/build/css> and #<path:/projects/build/>"
             (λ () (parameterize ([top "/projects/blog"]
                                  [current-output-dir "../build"])
                     (abs->rel/www (string->path "/not/projects/build/css"))))))

(define/doc (abs->rel/src [path absolute-path?] path-string?)
  @{Convert an absolute local path to a local path-string relative to
    @racket[src-path].}
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

(define/doc (abs->rel/top [path absolute-path?] path-string?)
  @{Convert an absolute local path to a local path-string relative to
    @racket[top].}
  (define segments (abs->rel 'abs->rel/top (simplify-path path) (build-path* (top))))
  (path->string (apply build-path segments)))

(module+ test
  (check-equal? (parameterize ([top "/projects/blog"])
                  (abs->rel/top (string->path "/projects/blog/foo.md")))
                "foo.md")
  (check-equal? (parameterize ([top "/projects/blog"])
                  (abs->rel/top (string->path "/projects/blog/source/foo.md")))
                "source/foo.md"))

(define (abs->rel who a b) ;; symbol? path? path? -> (listof path?)
  (define as (explode-path a))
  (define bs (explode-path b))
  (define-values (prefix tail _) (split-common-prefix as bs))
  (unless (equal? prefix bs)
    (error who "No common prefix: ~v and ~v" a b))
  tail)

(define/doc (canonical-uri [uri-path string?] string?)
  @{Possibly rewrite a URI path to take account of non-@racket[#f]
    @racket[current-uri-prefix] and @racket[uri-path-segment-encode]
    it.}
  #:ex [(require frog/paths frog/params)]
  #:ex [(canonical-uri "relative/λ/path")
        "relative/%CE%BB/path"]
  #:ex [(parameterize ([current-uri-prefix #f])
          (canonical-uri "/absolute/λ/path"))
        "/absolute/%CE%BB/path"]
  #:ex [(parameterize ([current-uri-prefix "/prefix"])
          (canonical-uri "/absolute/λ/path"))
        "/prefix/absolute/%CE%BB/path"]
  (define (reroot p) ;; (-> string? path-string?)
    (if (and (current-uri-prefix) (absolute-path? p))
        (build-path (current-uri-prefix) (path->relative-path p))
        p))
  (define (encode p) ;; (-> path-string? string?)
    (define encode-seg (compose1 uri-path-segment-encode path->string))
    (define segs
      (if (absolute-path? p)        ;don't encode the leading / or c:\
          (match (explode-path p) [(cons x xs) (cons x (map encode-seg xs))])
          (map encode-seg (explode-path p))))
    (path->string (apply build-path segs)))
  (define (dir? p)
    (define-values (_p _b dir?) (split-path p))
    dir?)
  (define (preserve-trailing-slash orig new)
    ;; Restore trailing slash lost by path->relative-path,
    ;; explode-path, etc.
    (string-append new (if (dir? orig) "/" "")))
  (preserve-trailing-slash uri-path
                           (encode (reroot uri-path))))

(module+ test
  (parameterize ([current-uri-prefix "/prefix///"])
    (check-equal? (canonical-uri "/a/λ/p") "/prefix/a/%CE%BB/p")
    (check-equal? (canonical-uri "/a/λ/p/") "/prefix/a/%CE%BB/p/")))

(define/doc (full-uri [uri-path string?] string?)
  @{Given a URI path string, prepend the scheme & host to make a full URI.}
  #:ex [(require frog/paths frog/params)]
  #:ex [(parameterize ([current-scheme/host "https://www.example.com"])
          (full-uri "/absolute/path/to/file.html"))
        "https://www.example.com/absolute/path/to/file.html"]
  (match uri-path
    [(pregexp #px"^/")
     (string-append (current-scheme/host) uri-path)]
    [_ (raise-user-error 'full-uri
                         "can't attach host/scheme to relative path")]))

(module+ test
  (parameterize ([current-scheme/host "https://www.example.com"])
    (check-exn exn:fail? (λ () (full-uri "relative/path/to/file.html")))))

(define/doc (slug [s string?] string?)
  @{Convert a string into a "slug", in which:
    @itemlist[@item{The string is Unicode normalized to NFC form using
                    @racket[string-normalize-nfc]}
              @item{Consecutive characters that are neither
                    @racket[char-alphabetic?] nor @racket[char-numeric?]
                    are replaced by hyphens.}
              @item{The string is Unicode normalized to NFD form using
                    @racket[string-normalize-nfd]}]}
  #:ex [(require frog/paths)]
  #:ex [(slug "Foo? Bar. Baz.")
        "Foo-Bar-Baz"]
  #:ex [(slug "Here's a question--how many hyphens???")
        "Here-s-a-question-how-many-hyphens"]
  #:ex [(slug "La biblioteca está en el estómago de Godzilla")
        "La-biblioteca-está-en-el-estómago-de-Godzilla"]
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
  (check-equal? (slug "'Foo, bar'") "-Foo-bar"))
