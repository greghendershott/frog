#lang racket/base

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

(require racket/require
         (for-syntax racket/base)
         frog/widgets
         (multi-in racket (contract dict function))
         web-server/templates)

(provide render-template)

;; Not a submodule. Avoid running the module at all.
(module test racket/base
  (require rackunit))

;; The modules needed by the template. Note that these must
;; be required above normally in this template.rkt module.
(define mods '(racket
               web-server/templates
               frog/widgets))

;; Create a namespace in which to evaluate templates, attach and
;; require the desired modules, and keep reusing it (faster).

(define (create-template-namespace)
  (define orig-ns (current-namespace))
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    ;; `namespace-attach-module` says the new namespace can reuse the
    ;; module already imported into orig-ns. Faster.
    (for-each (curry namespace-attach-module orig-ns) mods)
    ;; Require the files into the namespace, too. In the case of
    ;; racket, that's mandatory (sorta the #lang racket).  The others
    ;; we could `require` in the eval form, but simplest to handle
    ;; them here, too.
    (for-each namespace-require mods))
  ns)

(define template-namespace (create-template-namespace))

(define/contract (render-template dir filename dict)
  (path? path-string? dict? . -> . string?)
  (parameterize ([current-namespace template-namespace]
                 [current-load-relative-directory dir])
    ;; val a form. The `let` introduces the variables from
    ;; dict. `include/text` effectively evaluates the template as if
    ;; it were written in #lang scribble/text.
    (eval `(let (,@(for/list ([(k v) (in-dict dict)])
                     (list k v)))
             (include-template ,filename))
          (current-namespace))))
