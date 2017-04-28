#lang racket/base

(provide load
         init
         enhance-body
         clean)

(define (load top)
  (define frog.rkt (build-path top "frog.rkt"))
  (define-syntax-rule (-load what)
    (let ([f (dynamic-require frog.rkt 'what)])
      (when f (set! what f))))
  (-load init)
  (-load enhance-body)
  (-load clean))

(define-syntax-rule (def what)
  (define what (λ _
                 (error 'what
                        "not yet dynamic-required from frog.rkt"))))

(def init)
(def enhance-body)
(def clean)
