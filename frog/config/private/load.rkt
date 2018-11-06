#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/syntax))

(begin-for-syntax
  (define provide-syms '(init
                         enhance-body
                         clean))
  (provide provide-syms))

(define-syntax (define-the-things stx)
  (with-syntax ([(id ...) (map (curry format-id stx "~a") provide-syms)]
                [load     (format-id stx "load")])
    #'(begin
        (define id
          (λ _ (error 'id "not yet dynamic-required from frog.rkt"))) ...
        (provide id ...)

        (define (load top)
          (define frog.rkt (build-path top "frog.rkt"))
          (let ([fn (with-handlers ([exn:fail:filesystem? cannot-find-frog.rkt])
                      (dynamic-require frog.rkt 'id))])
            (when fn (set! id fn))) ...)
        (provide load))))

(define-the-things)

(define (cannot-find-frog.rkt . _)
  (eprintf "Cannot open frog.rkt.\nMaybe you need to `raco frog --init` ?\n")
  (exit 1))

(module+ test
  (require rackunit
           racket/runtime-path)
  (test-case "before loading example/frog.rkt"
    (check-exn #rx"init: not yet dynamic-required from frog.rkt"
               (λ () (init)))
    (check-exn #rx"enhance-body: not yet dynamic-required from frog.rkt"
               (λ () (enhance-body '((p () "hi")))))
    (check-exn #rx"clean: not yet dynamic-required from frog.rkt"
               (λ () (clean))))
  (define-runtime-path example "../../../example/")
  (test-case "after loading example/frog.rkt"
    (load example)
    (check-not-exn (λ () (init)))
    (check-not-exn (λ () (enhance-body '((p () "hi")))))
    (check-not-exn (λ () (clean)))))
