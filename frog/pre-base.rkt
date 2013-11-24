;; Like racket/base, but without `date`.
(module pre-base racket/base
  (provide (except-out (all-from-out racket/base) date)))
