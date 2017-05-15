#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         scribble/srcdoc
         (for-doc scribble/eval))

(provide define/doc)

;; Both `proc-doc` and `proc-doc/names` can be verbose and repetitious.
;; Provide an alternative with approximately the same signature as `defproc`.
;; (Caveat: Can only express ->* contracts, not ->i or ->d.)
;;
;; Also, provide a way to list expressions that are used both as doc
;; examples and as unit tests.

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
             #:with proc-doc #'(id default)))

  (define-splicing-syntax-class example/test
    #:attributes (example test)
    (pattern (~seq #:ex [actual:expr expected:expr])
             #:with example #'actual
             #:with test (syntax/loc #'actual ;helpful srcloc when tests fail
                           (check-equal? actual expected)))
    ;; This pattern is for example expressions like (require foo) that
    ;; aren't also a unit test.
    (pattern (~seq #:ex [actual:expr])
             #:with example #'actual
             #:with test #'(begin))))

(define-syntax (define/doc stx)
  (syntax-parse stx
    [(_ (id:id req:required-argument ...
               opt:optional-argument ...
               result-contract:expr)
        (doc-expr:expr ...)
        et:example/test ...
        body:expr ...+)
     (with-syntax ([((req-decl     ...) ...) #'(req.decl     ...)]
                   [((opt-decl     ...) ...) #'(opt.decl     ...)]
                   [((req-contract ...) ...) #'(req.contract ...)]
                   [((opt-contract ...) ...) #'(opt.contract ...)]
                   [(doc-examples ...) (syntax-parse #'(et.example ...)
                                         [()            #'()] ;avoid (examples)
                                         [(e:expr ...+) #'((examples e ...))])])
       (syntax/loc stx
         (begin
           (define (id req-decl ... ...
                       opt-decl ... ...)
             body ...)
           (module+ test
             (require rackunit)
             et.test ...)
           (provide
            (proc-doc/names id
                            (->* (req-contract ... ...)
                                 (opt-contract ... ...)
                                 result-contract)
                            ((req.proc-doc ...)
                             (opt.proc-doc ...))
                            (doc-expr ... doc-examples ...))))))]))
