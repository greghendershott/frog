#lang racket/base ;;#lang frog ??

(require frog/params
         frog/enhance-body
         frog/verbosity)

(provide init
         enhance-body
         clean)

;; init : -> Void
;;
;; Called early when Frog launches. Use this to set parameters defined
;; in frog/params.
(define (init)
  (void
   (displayln "My project's blog.rkt `init` was called.")))

;; enhance-body : (Listof Xexpr) -> (Listof Xexpr)
;;
;; Called once per post and non-post page, on the contents -- a list
;; of xexprs.
(define (enhance-body xs)
  ;; FIXME: Delete the parameters current-python-executable,
  ;; current-pygments-linenos?, and current-pygments-cssclass. Instead
  ;; have syntax-highlight take as args. Also probably rename it to
  ;; something like pgyments-highlight.
  (syntax-highlight
   ;; FIXME: Delete the current-embed-tweet-parents? parameter. Instead
   ;; auto-embed-tweets takes arg.
   (auto-embed-tweets
    ;; FIXME: Delete the parameters current-racket-doc-link-code? and
    ;; current-racket-doc-link-prose?. Instead racket-doc-links arg.
    (add-racket-doc-links xs))))

;; clean : -> Void
;;
;; Called from `raco frog --clean`. If your `enhance-body` generated
;; extra files, this is your chance to delete them.
(define (clean)
  (void
   (prn1 "My project's blog.rkt `clean` was called.")))
