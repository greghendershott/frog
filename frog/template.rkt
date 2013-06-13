#lang racket/base

(require racket/dict
         racket/contract
         web-server/templates
         frog/widgets)

(provide render-template)

;; Beware, cargo cults be here!
;;
;; The idea here is that we make some variables visible in the
;; namespace in which the template is evaluated. Some of these are the
;; variables and values explicitly passed to us in `dict`. The others
;; are from the modules web-server/templates and frog/widgets.
;;
;; Now, web-server/templates are normally used "statically" --
;; compiled into the web-server application. However it's also
;; possible to use them "dynamically" -- to load and use one at run
;; time.
;;
;; I wish I had a crisper understanding how and why the following
;; works, but it does, following this Racket mailing list thread:
;; http://www.mail-archive.com/users@racket-lang.org/msg18108.html

(define/contract (render-template dir filename dict)
  (path? path-string? dict? . -> . string?)
  (define template-namespace (make-empty-namespace))
  (define (attach/require m) ; symbol? -> void
    (namespace-attach-module (current-namespace) m template-namespace)
    (parameterize [(current-namespace template-namespace)]
      (namespace-require m)))
  (attach/require 'web-server/templates)
  (attach/require 'frog/widgets)
  (for ([(k v) (in-dict dict)])
    (namespace-set-variable-value! k v #f template-namespace))
  (define to-eval
    #`(include-template #,(datum->syntax #'render-template filename)))
  (parameterize ([current-directory dir])
    (eval to-eval template-namespace)))
