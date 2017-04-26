#lang racket/base

;; This is only used to read the deprecated .frogrc -- from which we attempt
;; to create an equivalent blog.rkt for users.

(require racket/dict
         racket/file
         racket/match
         "verbosity.rkt")

(provide get-config)

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
