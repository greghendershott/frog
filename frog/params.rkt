#lang racket/base

(require racket/match)

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; Parameters loaded from configuration file

(define current-scheme/host (make-parameter #f))
(define current-uri-prefix
  (make-parameter #f
                  (λ (v)
                    (and v
                         (match (regexp-replace #px"/+$" v "")
                           ["" #f]
                           [v v])))))
(define current-title (make-parameter #f))
(define current-author (make-parameter #f))
(define current-editor (make-parameter #f))
(define current-editor-command (make-parameter #f))
(define current-permalink (make-parameter #f))
(define current-index-full? (make-parameter #f)) ;index pages: full posts?
(define current-feed-full? (make-parameter #f))  ;feeds: full posts?
(define current-show-tag-counts? (make-parameter #t))
(define current-max-feed-items (make-parameter 999))
(define current-decorate-feed-uris? (make-parameter #t))
(define current-feed-image-bugs? (make-parameter #f))
(define current-auto-embed-tweets? (make-parameter #t))
(define current-embed-tweet-parents? (make-parameter #t))
(define current-racket-doc-link-code? (make-parameter #t))
(define current-racket-doc-link-prose? (make-parameter #f))
(define current-posts-per-page (make-parameter 10))
(define current-index-newest-first? (make-parameter #t))
(define current-posts-index-uri (make-parameter "/index.html"))
(define current-source-dir (make-parameter "_src"))
(define current-output-dir (make-parameter "."))
(define current-python-executable (make-parameter "python"))
(define current-pygments-linenos? (make-parameter #t))
(define current-pygments-cssclass (make-parameter "source"))
