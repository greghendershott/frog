#lang racket/base

;; Provide a form that's like `parameterize`, except:
;;
;; - Try to read the values from the specified configuration file. The
;; value supplied for each parameter in the form is used only as a
;; default if present in the config file.
;;
;; - The parameters are assumed to be named with a "current-"
;; prefix. But in the form, the parameter names are specified without
;; the "current-" prefix. Likewise the configuration file doesn't use
;; any "current-" prefix (would be unnecessarily verbose for users).
;;
;; - The defaults supplied in the form act as lite types: If the
;; default is a number, then the config file value must be a number.
;;
;; - Booleans may be specified in the config file as any of `#t`,
;; `#f`, `true`, or `false`.
;;
;; Example:
;;
;; (define current-twaddle-level (make-parameter 0))
;; (parameterize-from-config "foo.cfg"
;;                           ([twaddle-level 10])
;;    ....)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         racket/dict
         racket/file
         racket/match
         "verbosity.rkt")

(provide parameterize-from-config)

(module+ test
  (require rackunit))

(define-syntax (parameterize-from-config stx)
  (syntax-parse stx
    [(_ cfg-path:expr ([name:id default:expr] ...)
        body ...)
     (with-syntax ([(id ...) (stx-map (λ (x) (format-id stx "current-~a" x))
                                      #'(name ...))])
       #'(parameterize ([id (get-config (quote name) default cfg-path)] ...)
           body ...))]))

(define config #f) ;; (hash/c symbol? any/c)
(define (get-config name default cfg-path) ;; (symbol? any/c path? -> any/c)
  ;; Read all & memoize
  (unless config
    (set! config (read-config cfg-path)))
  (cond [(dict-has-key? config name)
         (define v (dict-ref config name))
         (cond [(string? default) v]
               [(boolean? default) v]
               [(number? default)
                (or (string->number v)
                    (begin
                      (eprintf
                       "Expected number for ~a. Got '~a'. Using default: ~a\n"
                       name v default)
                      default))]
               [else (raise-type-error 'get-config
                                       "string, boolean, or number"
                                       v)])]
        [else default]))

(define (read-config p)
  (cond [(file-exists? p)
         (prn0 "Using configuration ~a" (simplify-path p))
         (for/hasheq ([s (file->lines p)])
           (match s
             [(pregexp "^(.*)#?.*$" (list _ s))
              (match s
                [(pregexp "^\\s*(\\S+)\\s*=\\s*(.+)$" (list _ k v))
                 (values (string->symbol k) (maybe-bool v))]
                [else (values #f #f)])]
             [_ (values #f #f)]))]
        [else
         (prn0 "Configuration ~a not found; using defaults." p)
         (make-hasheq)]))

(define (maybe-bool v) ;; (any/c -> (or/c #t #f any/c))
  (match v
    [(or "true" "#t") #t]
    [(or "false" "#f") #f]
    [else v]))
