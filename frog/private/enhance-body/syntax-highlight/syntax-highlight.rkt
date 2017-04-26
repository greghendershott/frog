#lang rackjure/base

(require racket/match
         rackjure/str
         "pygments.rkt")

(provide syntax-highlight)

(define (syntax-highlight xs
                          #:python-executable python-executable
                          #:line-numbers? line-numbers?
                          #:css-class css-class)
  (for/list ([x xs])
    (match x
      [(or `(pre ([class ,brush]) (code () ,(? string? texts) ...))
           `(pre ([class ,brush]) ,(? string? texts) ...))
       (match brush
         [(pregexp "\\s*brush:\\s*(.+?)\\s*$" (list _ lang))
          `(div ([class ,(str "brush: " lang)])
                ,@(pygmentize (apply string-append texts) lang
                              #:python-executable python-executable
                              #:line-numbers? line-numbers?
                              #:css-class css-class))]
         [_ `(pre ,@texts)])]
      [x x])))
