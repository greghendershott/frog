#lang racket/base

(require racket/contract
         frog/frog
         frog/plugins)

(provide (contract-out
          [enhance-body ((any/c . -> . any/c) . -> . any)]
          [add-cleanup ((-> any) . -> . any)]))

(define enhance-body extend-enhance-body)
(define add-cleanup extend-clean)
