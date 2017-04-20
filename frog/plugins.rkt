#lang racket/base

(require setup/getinfo
         racket/function)

(provide init
         extend-clean
         clean
         extend-enhance-body
         enhance-body)

(module+ test
  (require rackunit))

(define clean-thunks '())
(define enhance-body-thunks '())

(define (init)
  (define plugins (find-relevant-directories '(frog-plugin)))
  (unless (null? plugins)
    (display "Adding plugins:")
    (for ([p plugins])
      (define info-proc (get-info/full p))
      (let ([mod (info-proc 'frog-plugin)])
        (printf " ~a" (info-proc 'frog-plugin-name))
        (dynamic-require (build-path p mod) #f)))
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-clean t)
  (set! clean-thunks (append clean-thunks (list t))))

(define (clean)
  (define apply0 (curryr apply '()))
  (for-each apply0 clean-thunks))

(module+ test
  (require racket/port)
  (extend-clean (thunk (display "foo")))
  (test-equal? "clean"
               (with-output-to-string clean)
               "foo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-enhance-body t)
  (set! enhance-body-thunks (append enhance-body-thunks (list t))))

;;; foldl??
(define (enhance-body xs)
  (define (do-it xs ts)
    (if (null? ts)
        xs
        (do-it ((car ts) xs) (cdr ts))))
  (do-it xs enhance-body-thunks))

(module+ test
  (extend-enhance-body
   (λ (xs)
     (map add1 xs)))
  (extend-enhance-body
   (λ (xs)
     (map sub1 xs)))
  (test-equal? "enhance body"
               (enhance-body '(1 2 3))
               '(1 2 3)))
