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
  (for ([t clean-thunks]) (t)))

(module+ test
  (require racket/port)
  (extend-clean (thunk (display "foo")))
  (test-equal? "clean"
               (with-output-to-string clean)
               "foo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-enhance-body t)
  (set! enhance-body-thunks (append enhance-body-thunks (list t))))

(define (enhance-body xs)
  (foldl (λ (proc v) (proc v)) xs enhance-body-thunks))

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
