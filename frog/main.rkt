#lang racket

(require (only-in "private/frog.rkt" main serve))

(provide serve)

(module+ main
  (main))
