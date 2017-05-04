#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/match
                     racket/syntax
                     syntax/parse)
         reprovide/reprovide)

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%provide
                     provide)
         (rename-out [our-#%module-begin #%module-begin]))

(reprovide racket/contract
           rackjure/threading
           xml/xexpr
           "../params.rkt"
           "../paths.rkt"
           "../enhance-body.rkt")

(define-syntax (our-#%module-begin stx)
  (syntax-parse stx
    [(_ forms ...)
     (define provide-syms '(init enhance-body clean))
     (with-syntax ([(provides ...) (map (curry format-id stx "~a") provide-syms)])
       ;; Simply? #'(#%module-begin forms ... (provide provides ...))
       ;;
       ;; But if the user has failed to define one of the mandatory functions
       ;; we want to supply a helpful error message that specifically
       ;; identifies it. And of course preserve stx srcloc so their editor of
       ;; choice can open their frog.rkt.
       (define (fail-sym exn)
         (match (exn:fail:syntax-exprs exn)
           [(cons stx _) (syntax-e stx)]
           [_            #f]))
       (with-handlers
         ([(λ (e)
             (and (exn:fail:syntax? e)
                  (regexp-match? #rx"provided identifier not defined or imported"
                                 (exn-message e))
                  (memq (fail-sym e) provide-syms)))
           (λ (e)
             (raise-syntax-error
              'frog/config
              (format "You must define a function named \"~a\"" (fail-sym e))
              stx))])
         (local-expand (syntax/loc #'(forms ...)
                         (#%module-begin forms ... (provide provides ...)))
                       'module-begin '())))]))

(module* test (submod "..")
  (define init #f)
  (define enhance-body #f)
  (define clean #f))
