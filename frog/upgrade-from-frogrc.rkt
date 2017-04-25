#lang racket/base

(require racket/match
         racket/pretty
         "params.rkt"
         "config.rkt"
         "verbosity.rkt")

(provide upgrade-from-frogrc)

(define (upgrade-from-frogrc top)
  (define blog.rkt (build-path top "blog.rkt"))
  (unless (file-exists? blog.rkt)
    (define frogrc (build-path top ".frogrc"))
    (get-config 'title "" frogrc) ;do now so messages not written to blog.rkt
    (prn0 "Creating blog.rkt from .frogrc -- see upgrade documentation.")
    (flush-output)
    (with-output-to-file #:mode 'text #:exists 'error
      blog.rkt (λ () (default frogrc)))))

(define (default frogrc)
  (define (pw v)
    (pretty-write v)
    (newline))
  (displayln "#lang racket/base\n")
  (pw `(require frog/params frog/enhance-body))
  (pw `(define (init)
         ,@(for/list ([x (in-list '([scheme/host "http://www.example.com"]
                                    [uri-prefix #f]
                                    [title "Untitled Site"]
                                    [author "The Unknown Author"]
                                    [editor "$EDITOR"]
                                    [editor-command "{editor} {filename}"]
                                    [show-tag-counts? #t]
                                    [permalink "/{year}/{month}/{title}.html"]
                                    [index-full? #f]
                                    [feed-full? #f]
                                    [max-feed-items 999]
                                    [decorate-feed-uris? #t]
                                    [feed-image-bugs? #f]
                                    [auto-embed-tweets? #t]
                                    [embed-tweet-parents? #t]
                                    [racket-doc-link-code? #t]
                                    [racket-doc-link-prose? #f]
                                    [posts-per-page 10]
                                    [index-newest-first? #t]
                                    [posts-index-uri "/index.html"]
                                    [source-dir "_src"]
                                    [output-dir "."]
                                    [python-executable "python"]
                                    [pygments-linenos? #t]
                                    [pygments-cssclass "source"]))])
             (match-define (list sym def) x)
             `(,(string->symbol (format "current-~a" sym))
               ,(get-config sym def frogrc)))))
  (pw `(define (enhance-body xs)
         (auto-embed-tweets (add-racket-doc-links (syntax-highlight xs)))))
  (pw `(define (clean)
         (void))))

(default "/Users/greg/src/racket/collects/frog/example/.frogrc")
