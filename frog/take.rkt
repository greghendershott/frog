#lang racket/base

(require racket/list)

(provide take<=
         split-at<=
         take-every)

;; Like `take`, but OK if list has fewer than `n` members.
(define (take<= xs n)
  (for/list ([x (in-list xs)]
             [_ (in-range n)])
    x))

;; Like `split-at`, but OK if list has fewer than `n` members.
(define (split-at<= xs n)
  (cond [(<= n (length xs)) (split-at xs n)]
        [else (values xs '())]))

(require rackunit)
(let ()
  (define-values (a b) (split-at<= '(1 2 3 4 5) 2))
  (check-equal? a '(1 2))
  (check-equal? b '(3 4 5)))
(let ()
  (define-values (a b) (split-at<= '() 1))
  (check-equal? a '())
  (check-equal? b '()))
(let ()
  (define-values (a b) (split-at<= '(1) 1))
  (check-equal? a '(1))
  (check-equal? b '()))

;; Run `split-at` over a list, producing a list of lists, each of
;; which has `n` elements (except possibly the last one).
(define (take-every xs n)
  (define-values (this more) (split-at<= xs n))
  (cond [(empty? more) (list this)]
        [else (cons this (take-every more n))]))

(check-equal? (take-every '(1 2 3 4 5) 2)
              '((1 2) (3 4) (5)))
(check-equal? (take-every '() 1)
              '(()))
(check-equal? (take-every '(1) 1)
              '((1)))
