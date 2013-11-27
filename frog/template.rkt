#lang racket

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

(require web-server/templates
         frog/widgets)

;; The modules needed by the template. Note that these must
;; be required above normally in this template.rkt module.
(define mods '(racket
               web-server/templates
               frog/widgets))

(define/contract (render-template dir filename dict)
  (path? path-string? dict? . -> . string?)
  (define orig-ns (current-namespace))
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-load-relative-directory dir])
    ;; `namespace-attach-module` says the new namespace can reuse the
    ;; module already imported into orig-ns. Faster.
    (for-each (curry namespace-attach-module orig-ns) mods)
    ;; Require the files into the namespace, too. In the case of
    ;; racket, that's mandatory (sorta the #lang racket).  The others
    ;; we could `require` in the eval form, but simplest to handle
    ;; them here, too.
    (for-each namespace-require mods)
    ;; And finally, eval a form. The `let` introduces the variables
    ;; from dict. `include/text` effectively evaluates the template as
    ;; if it were written in #lang scribble/text.
    (eval `(let (,@(for/list ([(k v) (in-dict dict)])
                     (list k v)))
             (include-template ,filename))
          (current-namespace))))

(provide render-template)
