#lang racket/base

(require racket/contract
         frog/plugins)

(provide (contract-out
          [get-config (symbol? any/c . -> . any/c)]
          [rename extend-enhance-body enhance-body ((any/c . -> . any/c) . -> . any)]
          [rename extend-clean register-cleanup ((-> any) . -> . any)]))
