#lang racket/base

(module+ test
  (require rackunit)

  (check-not-exn (λ () (eval-syntax #'(module m frog/config/main
                                        (define init #f)
                                        (define enhance-body #f)
                                        (define clean #f))
                                    (make-base-empty-namespace))))

  (check-exn #rx"frog/config: You must define a function named \"init\""
             (λ () (eval-syntax #'(module m frog/config/main
                                    #;(define init #f)
                                    (define enhance-body #f)
                                    (define clean #f))
                                (make-base-empty-namespace))))

  (check-exn #rx"frog/config: You must define a function named \"enhance-body\""
             (λ () (eval-syntax #'(module m frog/config/main
                                    (define init #f)
                                    #;(define enhance-body #f)
                                    (define clean #f))
                                (make-base-empty-namespace))))

  (check-exn #rx"frog/config: You must define a function named \"clean\""
             (λ () (eval-syntax #'(module m frog/config/main
                                    (define init #f)
                                    (define enhance-body #f)
                                    #;(define clean #f))
                                (make-base-empty-namespace)))))
