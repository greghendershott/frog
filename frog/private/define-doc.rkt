#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         scribble/srcdoc)

(provide define/doc)

;; Both `proc-doc` and `proc-doc/names` can be verbose and repetitious.
;; Provide an alternative with approximately the same signature as `defproc`.
;;
;; Caveat: Can only express ->* contracts, not ->i or ->d.

(begin-for-syntax
  (define-syntax-class required-arg
   #:attributes (id decl contract proc-doc)
   (pattern [id:id c:expr]
            #:with decl #'(id)
            #:with contract #'(c)
            #:with proc-doc #'id)
   (pattern [kw:keyword id:id c:expr]
            #:with decl #'(kw id)
            #:with contract #'(kw c)
            #:with proc-doc #'id))

  (define-syntax-class optional-arg
    #:attributes (id decl contract proc-doc)
    (pattern [id:id c:expr default:expr]
             #:with decl #'((id default))
             #:with contract #'(c)
             #:with proc-doc #'(id default))
    (pattern [kw:keyword id:id c:expr default:expr]
             #:with decl #'(kw (id default))
             #:with contract #'(kw c)
             #:with proc-doc #'(id default))))

(define-syntax (define/doc stx)
  (syntax-parse stx
   [(_ (id:id req:required-arg ... opt:optional-arg ... result-contract)
       doc:expr
       body:expr ...+)
    (define/syntax-parse ((req-decl     ...) ...) #'(req.decl ...))
    (define/syntax-parse ((opt-decl     ...) ...) #'(opt.decl ...))
    (define/syntax-parse ((req-contract ...) ...) #'(req.contract ...))
    (define/syntax-parse ((opt-contract ...) ...) #'(opt.contract ...))
    (syntax/loc stx
      (begin
        (define (id req-decl ... ... opt-decl ... ...)
          body ...)
        (provide
         (proc-doc/names id
                         (->* (req-contract ... ...)
                              (opt-contract ... ...)
                              result-contract)
                         ((req.proc-doc ...)
                          (opt.proc-doc ...))
                         doc))))]))
