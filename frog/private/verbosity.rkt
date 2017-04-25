#lang racket/base

(require (for-syntax racket/base racket/syntax))

(provide current-verbosity prn0 prn1 prn2)

(module+ test
  (require rackunit))

;; Note: This may be set to -1 for silent mode.
(define current-verbosity (make-parameter 0))

(define (prn level fmt . vs)
  (when (>= (current-verbosity) level)
    (apply printf fmt vs)
    (newline)))

(define-syntax (define-prn stx)
  (syntax-case stx ()
    [(_ level)
     (with-syntax ([id (format-id stx "prn~a" (syntax-e #'level))])
       #'(define (id fmt . vs)
           (apply prn level fmt vs)))]))

(define-prn 0)
(define-prn 1)
(define-prn 2)
