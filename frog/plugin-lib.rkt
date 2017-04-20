#lang racket/base

(require setup/getinfo)

;; (provide init-plugins extend-clean extend-enhance-body)
(provide extend-clean2)

(module+ test
  (require rackunit))

(define clean-thunks '())
(define enhance-body-thunks '())

#; (define (init-plugins)
  (define plugins (find-relevant-directories '(frog-plugin)))
  (unless (null? plugins)
    (display "Adding plugins:")
    (for ([p plugins])
      (define info-proc (get-info/full p))
      (let ([mod (info-proc 'frog-plugin)])
        (printf " ~a" (info-proc 'frog-plugin-name))
        (dynamic-require (build-path p mod) #f)))
    (newline)))

(define (extend-clean2 t)
  (set! clean-thunks (cons t clean-thunks)))

(define (extend-enhance-body t)
  (set! enhance-body-thunks (cons t enhance-body-thunks)))

(module+ test
  (define thunk (λ () (void)))
  (extend-clean thunk)
  (check-equal? clean-thunks (list thunk)))
