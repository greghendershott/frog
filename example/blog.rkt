#lang racket/base

(require frog/params
         frog/enhance-body
         racket/contract
         rackjure/threading
         xml/xexpr)

(provide init
         enhance-body
         clean)

;; Called early when Frog launches. Use this to set parameters defined
;; in frog/params.
(define/contract (init)
  (-> any)
  (current-scheme/host "http://www.example.com")
  (current-title "My Blog")
  (current-author "The Unknown Author"))

;; Called once per post and non-post page, on the contents.
(define/contract (enhance-body xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  ;; Here we pass the xexprs through a series of functions.
  (~> xs
      (syntax-highlight #:python-executable "python"
                        #:line-numbers? #t
                        #:css-class "source")
      (auto-embed-tweets #:parents? #t)
      (add-racket-doc-links #:code? #t #:prose? #f)))

;; clean : -> Void
;;
;; Called from `raco frog --clean`.
;;
;; In `enhance-body`, you can call a function that has the side-effect
;; of creating extra files (for example responsive images in a variety
;; of sizes). Such a function should provide a companion you can call
;; to delete those files; call it here.
(define/contract (clean)
  (-> any)
  (void))
