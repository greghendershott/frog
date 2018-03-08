#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/match
         racket/runtime-path
         "../paths.rkt")

(provide (all-from-out "../paths.rkt")
         example
         permalink-path
         post-path->link
         editor-command-string)

(module+ test
  (require rackunit
           racket/function))

;; For interactive development and for --init feature.
(define-runtime-path example "../../example/")

(define/contract (permalink-path year month day title filename pattern)
  (-> string? string? string? string? string? string? path?)
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
  (-> path? string?)
  (match (abs->rel/www pp) ;assumes abs->rel/www always returns 'unix style
    [(pregexp "^(.+?)/index.html" (list _ s)) (string-append s "/")]
    [s s]))

(module+ test
  (parameterize ([top (find-system-path 'home-dir)])
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post.html"))
     "/blog/2012/05/31/title-of-post.html")
    (check-equal?
     (post-path->link (build-path (top) "blog/2012/05/31/title-of-post/index.html"))
     "/blog/2012/05/31/title-of-post/")))

(define/contract (editor-command-string editor filename pattern)
  (-> string? string? string? string?)
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
