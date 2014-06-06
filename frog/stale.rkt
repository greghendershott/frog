#lang racket/base

(provide stale?)

;; (path? path? ... -> boolean?)
(define (stale? target . deps)
  (or (not (file-exists? target))
      (let ([target-secs (file-or-directory-modify-seconds target)])
        (for/or ([dep (in-list deps)])
          (< target-secs (file-or-directory-modify-seconds dep))))))
