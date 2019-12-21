#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/match
                     racket/syntax
                     syntax/parse)
         reprovide/reprovide
         "private/load.rkt")

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     #%provide
                     provide)
         (rename-out [our-#%module-begin #%module-begin]))

(reprovide racket/contract/base
           racket/contract/region
           threading
           xml/xexpr
           "../params.rkt"
           "../paths.rkt"
           "../enhance-body.rkt")

(define-syntax (our-#%module-begin stx)
  (syntax-parse stx
    [(_ forms ...)
     (with-syntax ([(provides ...) (map (curry format-id stx "~a") provide-syms)])
       ;; If we didn't care about error messages, this could simply be:
       ;;
       ;; #'(#%module-begin forms ... (provide provides ...))
       ;;
       ;; But if the user has failed to define one of the mandatory functions
       ;; we want to supply a helpful error message that specifically
       ;; identifies it. And of course preserve their frog.rkt stx srcloc.
       ;;
       ;; So we local-expand their forms, catch the appropriate syntax errors,
       ;; and re-raise them in a more-helpful form.
       (define (fail-sym exn)
         (match (exn:fail:syntax-exprs exn)
           [(cons stx _) (syntax-e stx)]
           [_            #f]))
       (define (provided-identifier-message? e)
         (define msg (exn-message e))
         (define rxs '(#rx"provided identifier not defined or imported"      ;v6
                       #rx"provided identifier is not defined or required")) ;v7
         (for/or ([rx (in-list rxs)])
           (regexp-match? rx msg)))
       (with-handlers
         ([(λ (e)
             (and (exn:fail:syntax? e)
                  (provided-identifier-message? e)
                  (memq (fail-sym e) provide-syms)))
           (λ (e)
             (raise-syntax-error
              'frog/config
              (format "You must define a function named \"~a\"" (fail-sym e))
              stx))])
         (local-expand (syntax/loc #'(forms ...)
                         (#%module-begin forms ... (provide provides ...)))
                       'module-begin '())))]))
