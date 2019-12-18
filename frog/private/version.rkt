#lang racket/base

(require racket/file
         racket/match
         racket/path
         racket/runtime-path
         setup/getinfo
         (for-syntax racket/base syntax/parse))

(provide frog-version)

(define (frog-version)
  (trying (version-from-get-info)
          (version-from-info.txt-file)
          "version can't be found from package info.rkt"))

(define (version-from-get-info)
  ((get-info/full "frog") 'version))

;; As fallback for some older versions of Racket, try the ugly hack of
;; regexp-ing info.rkt as text.
(define-runtime-path info.rkt "../../info.rkt")
(define (version-from-info.txt-file . _)
  (match (file->string info.rkt #:mode 'text)
    [(pregexp "^#lang info\n+\\(define version \"([^\"]+)\""
              (list _ v))
     v]))

;;; trying

(define-syntax (trying stx)
  (syntax-parse stx
    [(_ e:expr)
     #'e]
    [(_ e:expr more:expr ...)
     #'(with-handlers ([exn:fail? (λ _ (trying more ...))])
         e)]))

(module+ test
  (require rackunit)
  (check-equal? (trying 42) 42)
  (check-equal? (trying (error '0) 42) 42)
  (check-equal? (trying (error '0) (error '1) 42) 42)
  (check-equal? (trying 42 (error '0)) 42))
