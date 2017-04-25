#lang racket/base

(provide load
         init
         enhance-body
         clean)

(define (load top)
  (define blog.rkt (build-path top "blog.rkt"))
  (define-syntax-rule (-load what)
    (let ([f (dynamic-require blog.rkt 'what)])
      (when f (set! what f))))
  (-load init)
  (-load enhance-body)
  (-load clean))

(define-syntax-rule (def what)
  (define what (λ _
                 (error 'what
                        "not yet dynamic-required from blog.rkt"))))

(def init)
(def enhance-body)
(def clean)
