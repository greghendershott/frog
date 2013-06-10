#lang rackjure

(require xml "take.rkt")

(provide xexprs->description
         xexpr->markdown)

(module+ test
  (require rackunit))

(define (xexprs->description xs [num 3])
  (str (string-join (map xexpr->markdown (take<= xs num)))
       " ..."))

(module+ test
  (check-equal?
   (xexprs->description '((h1 ([class "foo"]) "A heading")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff."))
                        3)
   "A heading: A _paragraph_ of some stuff. A _paragraph_ of some stuff. ...")
  (check-equal?
   (xexprs->description '((h1 ([class "foo"]) "A heading")
                          (p "A " (em "paragraph") " of some stuff.")
                          (img ([src "blah"]))
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff."))
                        3)
   "A heading: A _paragraph_ of some stuff.  ..."))

;; Not full markdown, just a "lite" variation.
(define (xexpr->markdown x [p-suffix ""])
  (define (heading? s)
    (memq s '(h1 h2 h3 h4 h5 h6 h7 h8 h9)))
  (define (recur x)
    (xexpr->markdown x p-suffix))
  (define (->s es)
    (apply str (map recur es)))
  (match x
    [`(em            ([,_ ,_] ...) ... ,es ...) (str "_" (->s es) "_")]
    [`(strong        ([,_ ,_] ...) ... ,es ...) (str "**" (->s es) "**")]
    [`(,(? heading?) ([,_ ,_] ...) ... ,es ...) (str (->s es) ":")]
    [`(p             ([,_ ,_] ...) ... ,es ...) (str (->s es) p-suffix)]
    [`(,(? symbol?)  ([,_ ,_] ...) ... ,es ...) (str (->s es))]
    [(? string? s) s]
    ['ndash "--"]
    ['mdash "--"]
    ['amp "&"]
    [(or 'lsquo 'rsquo) "'"]
    [(or 'ldquo 'rdquo) "&quot;"]
    [(? valid-char? c) (integer->char c)]
    [else ""])) ;; ignore others

(module+ test
  (check-equal? (xexpr->markdown '(em "italic"))
                "_italic_")
  (check-equal? (xexpr->markdown '(em ([class "foo"]) "italic"))
                "_italic_")
  (check-equal? (xexpr->markdown '(strong "bold"))
                "**bold**")
  (check-equal? (xexpr->markdown '(strong ([class "foo"]) "bold"))
                "**bold**")
  (check-equal? (xexpr->markdown '(em "italic " (strong "bold") " italic"))
                "_italic **bold** italic_")
  (check-equal? (xexpr->markdown '(p "I am some " (em "italic") " text"))
                "I am some _italic_ text")
  (check-equal? (xexpr->markdown '(p ([class "foo"])
                                     "I am some " (em "italic") " text"))
                "I am some _italic_ text")
  (check-equal? (xexpr->markdown '(p "M" 'amp "Ms" 'mdash "gotta love 'em"))
                "M&Ms--gotta love 'em")
  (check-equal? (xexpr->markdown '(div (p "Hi.") (p "Hi.")) "\n")
                "Hi.\nHi.\n")
  (check-equal? (xexpr->markdown '(p "Hi" #x20 "there"))
                "Hi there"))
