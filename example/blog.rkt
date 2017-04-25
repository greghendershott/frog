#lang racket/base

(require frog/params
         frog/enhance-body
         frog/verbosity
         rackjure/threading)

(provide init
         enhance-body
         clean)

;; init : -> Void
;;
;; Called early when Frog launches. Use this to set parameters defined
;; in frog/params.
(define (init)
  (displayln "My project's blog.rkt `init` was called.")
  (current-scheme/host "http://www.example.com"))

;; enhance-body : (Listof Xexpr) -> (Listof Xexpr)
;;
;; Called once per post and non-post page, on the contents -- a list
;; of xexprs.
(define (enhance-body xs)
  (~> xs
      (syntax-highlight #:python-executable "python"
                        #:line-numbers? #t
                        #:css-class "source")
      (auto-embed-tweets #:parents? #t)
      (add-racket-doc-links #:code? #t #:prose? #f)))

;; clean : -> Void
;;
;; Called from `raco frog --clean`. If your `enhance-body` generated
;; extra files, this is your chance to delete them.
(define (clean)
  (void
   (prn1 "My project's blog.rkt `clean` was called.")))
