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
  (define (get k d) (get-config k d frogrc))
  (define (pw v)
    (pretty-write v)
    (newline))
  (displayln "#lang racket/base\n")
  (pw `(require frog/params frog/enhance-body rackjure/threading))
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
                                    [posts-per-page 10]
                                    [index-newest-first? #t]
                                    [posts-index-uri "/index.html"]
                                    [source-dir "_src"]
                                    [output-dir "."]))])
             (match-define (list sym def) x)
             `(,(string->symbol (format "current-~a" sym))
               ,(get sym def)))))
  ;; Some things in .frogrc that we used to read into global
  ;; parameters, but now are simply passed as arguments to functions.
  (define python-executable (get 'python-executable "python"))
  (define pygments-linenos? (get 'pygments-linenos? #t))
  (define pygments-cssclass (get 'pygments-cssclass "source"))
  (define auto-embed-tweets? (get 'auto-embed-tweets? #t))
  (define embed-tweet-parents? (get 'embed-tweet-parents? #t))
  (define racket-doc-link-code? (get 'racket-doc-link-code? #t))
  (define racket-doc-link-prose? (get 'racket-doc-link-prose? #f))
  (pw `(define (enhance-body xs)
         (~> xs
             (syntax-highlight #:python-executable ,python-executable
                               #:line-numbers? ,pygments-linenos?
                               #:css-class ,pygments-cssclass)
             ,@(if auto-embed-tweets?
                   `((auto-embed-tweets #:parents? ,embed-tweet-parents?))
                   `())
             ,@(if (or racket-doc-link-code? racket-doc-link-prose?)
                   `((add-racket-doc-links #:code? ,racket-doc-link-code?
                                           #:prose? ,racket-doc-link-prose?))
                   `()))))
  (pw `(define (clean)
         (void))))

;; (default "/Users/greg/src/racket/collects/frog/example/.frogrc")


