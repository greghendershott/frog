#lang racket

(require (for-syntax racket/syntax)
         "verbosity.rkt")

(provide parameterize-from-config)

(define config #f) ;; symbol? => any/c

(define (maybe-read-config p)
  (unless config
    (set! config
          (cond [(file-exists? p)
                 (prn0 "Using configuration ~a" p)
                 (for/hasheq ([s (file->lines p)])
                   (match s
                     [(pregexp "^(.*)#?.*$" (list _ s))
                      (match s
                        [(pregexp "^\\s*(\\S+)\\s*=\\s*(.+)$" (list _ k v))
                         (values (string->symbol k) (maybe-bool v))]
                        [else (values #f #f)])]
                     [else (values #f #f)]))]
                [else
                 (prn0 "Configuration ~a not found; using defaults." p)
                 (make-hasheq)]))))

(define (maybe-bool v) ;; (any/c -> (or/c #t #f any/c))
  (match v
    [(or "true" "#t") #t]
    [(or "false" "#f") #f]
    [else v]))

(define (get-config name default cfg-path) ;; (symbol? any/c path? -> any/c)
  (maybe-read-config cfg-path)
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

(define-syntax (parameterize-from-config stx)
  (syntax-case stx ()
    [(_ cfg-path ([name default] ...)
        body ...)
     (map identifier? (syntax->list #'(name ...)))
     (with-syntax ([(id ...) (map (lambda (x)
                                    (format-id stx "current-~a" x))
                                  (syntax->list #'(name ...)))])
       #'(parameterize ([id (get-config (quote name) default cfg-path)] ...)
           body ...))]))
