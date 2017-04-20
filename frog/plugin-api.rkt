#lang racket/base

(require frog/frog
         frog/plugins)

(provide (rename-out [api-extend-clean extend-clean])
         #;extend-enhance-body
         enhance-body
         )

(module+ test
  (require rackunit))

(define api-extend-clean extend-clean)

(define enhance-body extend-enhance-body)

;; (define (extend-enhance-body t)
;;   (set! enhance-body-thunks (cons t enhance-body-thunks)))
