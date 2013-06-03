#lang racket/base

(provide take<=)

;; Like `take`, but OK if list has fewer than `n` members.
(define (take<= xs n)
  (for/list ([x (in-list xs)]
             [_ (in-range n)])
    x))
