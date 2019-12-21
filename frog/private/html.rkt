#lang racket/base

(require racket/match
         threading
         xml
         (only-in html read-html-as-xml))

(provide read-html-as-xexprs)

(define (read-html-as-xexprs) ;; (-> (listof xexpr?))
  (~>> (read-html-as-xml)
       (element #f #f 'root '())
       xml->xexpr
       decode-ampersands-in-attributes
       cddr))

(define (decode-ampersands-in-attributes x)
  (match x
    [`(,tag ([,ks ,vs] ...) . ,els)
     `(,tag
       ,(for/list ([k ks]
                   [v vs])
          (list k (regexp-replace* "&amp;" v "\\&")))
       ,@(map decode-ampersands-in-attributes els))]
    [v v]))

(module+ test
  (require rackunit
           racket/port)
  (define input
    "<p>Hi</p>
<p><a href='/path/to/thing?a=1&b=2'>link</a></p>
<p><a href='/path/to/thing?a=1&amp;b=2'>link</a></p>")
  (check-equal? (with-input-from-string input read-html-as-xexprs)
                '((p () "Hi") "\n"
                  (p () (a ((href "/path/to/thing?a=1&b=2")) "link")) "\n"
                  (p () (a ((href "/path/to/thing?a=1&b=2")) "link")))))
