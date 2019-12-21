#lang racket/base

(require racket/require
         (multi-in racket (function list match)))

(provide xexpr-map)

;; Does depth-first traversal of the xexpr `x`, calling `f` for each
;; sub-xexpr of `x` (including `x` itself).
;;
;; The xexprs passed to `f` always have an explicit attributes list --
;; similar to `(parameterize ([xexpr-drop-empty-attributes
;; #f]))`. Normalizing them like this simplifies matching.
;;
;; Note that `f` must return a `(listof xexpr?)`, not an
;; `xexpr?`. This permits `f` to replace one xexpr with several that
;; get spliced in where the original one was (without needing to nest
;; them in some artificial parent element like a `span` or `div`). For
;; example: Can replace '(em "foo" "bar") with "foo" "bar". Of course,
;; this means if `f` wants to return the xexpr unchanged, it must be
;; nested in list: `(list original)` not `original`.  To delete an
;; xexpr completely, `f` can return the empty list '().
;;
;; `f` is also passed a `(listof xexpr?)` that is the parent xexprs,
;; in "reverse" order (parent, grandparent, and so on). This allows
;; transforming elements that are only descendants of specific other
;; elements. Note that these are the full xexprs; if you only care
;; about the tag symbols, you can run the list through (map first
;; parents) to get something like `(td tr tbody table)` or whatever.
;; To match on both the xexpr and the parents, you may find `match*`
;; handy.
(define (xexpr-map f x)
  ;;((xexpr/c (listof xexpr/c) . -> . (listof xexpr/c)) xexpr/c . -> . xexpr/c)
  (define (inner ps f x)
    (match x
      ;; xexpr with explicit attributes (even if just '())
      [`(,(? symbol? tag) ([,(? symbol? ks) ,(? string? vs)] ...) . ,es)
       (f `(,tag
            ,(map list ks vs)
            ,@(append* (map (curry inner (cons x ps) f)
                            es)))
          ps)]
      ;; xexpr with no attributes: transform to empty list
      [`(,(? symbol? tag) . ,es) (inner ps f `(,tag () ,@es))]
      [x (f x ps)]))
  (append* (inner '() f x)))

(module+ test
  (require rackunit)
  (define x1 '(html ()
                    (head ())
                    (body ()
                          (p ([class "foo"])
                             "hi" 32 (em () "M" amp "M" rsquo)
                             (strong () "ma") "there"))))

  (check-equal? (xexpr-map (lambda (x _) (list x))
                           x1)
                x1
                "identity")

  (check-equal?
   (xexpr-map (lambda (xexpr _)
                (match xexpr
                  ;; Change p to div
                  [`(p ,as . ,es) `((div ,as ,@es))]
                  ;; Replace (em x ...) with x ...
                  [`(em ,_ . ,es) `(,@es)]
                  ;; Delete (strong _ ...) completely
                  [`(strong . ,_) `()]
                  ;; Remains as-is.
                  [x `(,x)]))
              x1)
   '(html ()
          (head ())
          (body ()
                (div ([class "foo"])
                     "hi" 32 "M" amp "M" rsquo "there"))))

  (check-equal?
   (xexpr-map (lambda (xexpr parents)
                (match* (xexpr parents)
                  [(`(p ,as . ,es) (list `(div . ,_) _ ...))
                   '("DIRECTLY UNDER DIV")]
                  [(`(p ,as . ,es) (list-no-order `(div . ,_) _ ...))
                   '("INDIRECTLY UNDER DIV")]
                  [(_ _) (list xexpr)]))
              '(div (p "Directly under div")
                    (span (p "Indirectly under div"))))
   '(div () "DIRECTLY UNDER DIV" (span () "INDIRECTLY UNDER DIV")))

  (define (html->markdown/naive x p)
    (match x
      [`(em ,as . ,es)     `("_" ,@es "_")]
      [`(strong ,as . ,es) `("**" ,@es "**")]
      [`(p ,as . ,es)      `((,@es "\n"))]
      [_ `(,x)]))
  (check-equal?
   (apply string-append
          (xexpr-map html->markdown/naive
                     '(p "Nested " (em "italic " (strong "bold")) " stuff")))
   "Nested _italic **bold**_ stuff\n"))
