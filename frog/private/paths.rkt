#lang racket/base

(require racket/require
         (multi-in racket (contract format match runtime-path))
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

(define/contract (permalink-path d title filename pattern)
  (-> date? string? string? string? path?)
  (define (~2d n)
    (~a #:min-width       2
        #:align           'right
        #:left-pad-string "0"
        n))
  (build-path (www-path)
              (regexp-replaces pattern
                               `([#rx"{year}" ,(~a (date-year d))]
                                 [#rx"{month}" ,(~2d (date-month d))]
                                 [#rx"{day}" ,(~2d (date-day d))]
                                 [#rx"{title}" ,title]
                                 [#rx"{filename}",filename]
                                 [#rx"^/" ""]))))

(module+ test
  (parameterize ([top (find-system-path 'home-dir)])
    (define dt (date 0 0 0 31 5 2012 0 0 #f 0))
    (define f (curry permalink-path dt "title-of-post" "file-name"))
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
