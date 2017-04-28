#lang racket

(require (only-in "private/main.rkt" main serve))

(provide serve)

(module+ main
  (main))
