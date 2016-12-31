#lang racket/base

(require (only-in scribble/core style)
         (only-in scribble/manual para)
         (only-in scribble/html-properties attributes alt-tag)
         (only-in scribble/base literal))

(provide pygment-code)

(module+ test
  (require rackunit))

;; For use in Scribble source file. Lets you create a <pre> block with
;; a language tag that can be syntax highlighted by Pygments.
;;
;; Example usage:
;;
;; @pygment-code[#:lang "js"]{function foo() {return 1;}}
;;
(define (pygment-code #:lang lang . xs)
  (para #:style (style "brush:"
                       (list (attributes `([class . ,lang]))
                             (alt-tag "pre")))
        (apply literal xs)))
