#lang info
(define raco-commands '(("frog" (submod frog/frog main) "run Frog" #f)))
(define scribblings '(("frog.scrbl" (multi-page))))
(define clean '("compiled" "doc" "doc/frog"))
