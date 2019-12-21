#lang racket/base

(require racket/file
         racket/match
         racket/path
         racket/runtime-path
         setup/getinfo
         syntax/parse/define)

(provide frog-version)

(define (frog-version)
  (or/fail ((get-info/full "frog") 'version)
           ((get-info/full "../..") 'version)
           (version-from-info.txt-file)
           "-- version cannot be found from the Frog package's info.rkt"))

;; As a fallback try the ugly hack of regexp-ing info.rkt as text.
(define-runtime-path info.rkt "../../info.rkt")
(define (version-from-info.txt-file . _)
  (match (file->string info.rkt #:mode 'text)
    [(pregexp "^#lang info\n+\\(define version \"([^\"]+)\""
              (list _ v))
     v]))

;;; or/fail

(define-simple-macro (fail->false e:expr)
  (with-handlers ([exn:fail? (λ _ #f)]) e))

(define-simple-macro (or/fail e:expr ...)
  (or (fail->false e) ...))

(module+ test
  (require rackunit)
  (check-equal? (or/fail 42) 42)
  (check-equal? (or/fail (error '0) 42) 42)
  (check-equal? (or/fail (error '0) (error '1) 42) 42)
  (check-equal? (or/fail 42 (error '0)) 42))
