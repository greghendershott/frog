#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         scribble/srcdoc)

(provide define/doc)

;; Both `proc-doc` and `proc-doc/names` can be verbose and repetitious.
;; Provide an alternative with approximately the same signature as `defproc`.
;;
;; Caveat: Can only express ->* contracts, not ->i or ->d.

(begin-for-syntax
  (define-syntax-class required-argument
   #:attributes (decl contract proc-doc)
   (pattern [id:id c:expr]
            #:with decl #'(id)
            #:with contract #'(c)
            #:with proc-doc #'id)
   (pattern [kw:keyword id:id c:expr]
            #:with decl #'(kw id)
            #:with contract #'(kw c)
            #:with proc-doc #'id))

  (define-syntax-class optional-argument
    #:attributes (decl contract proc-doc)
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
    [(_ (id:id req:required-argument ...
               opt:optional-argument ...
               result-contract:expr)
        (doc-expr:expr ...)
        body:expr ...+)
     (with-syntax ([((req-decl     ...) ...) #'(req.decl     ...)]
                   [((opt-decl     ...) ...) #'(opt.decl     ...)]
                   [((req-contract ...) ...) #'(req.contract ...)]
                   [((opt-contract ...) ...) #'(opt.contract ...)])
       (syntax/loc stx
         (begin
           (define (id req-decl ... ...
                       opt-decl ... ...)
             body ...)
           (provide
            (proc-doc/names id
                            (->* (req-contract ... ...)
                                 (opt-contract ... ...)
                                 result-contract)
                            ((req.proc-doc ...)
                             (opt.proc-doc ...))
                            (doc-expr ...))))))]))
