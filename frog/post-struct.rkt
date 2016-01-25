#lang racket/base

(provide (struct-out post)
         post-file-px)

(module+ test
  (require rackunit))

(struct post (title      ;string?
              src-path   ;path? - full pathname of source file
              modified   ;exact-integer? time blurb/body were updated
              dest-path  ;path? - full pathname of local HTML file
              uri-path   ;string? - path portion of URI, with leading /
              date       ;string? - 8601 datetime format
              older      ;(or/c path? #f) - the src-path of older post
              newer      ;(or/c path? #f) - the src-path of newer post
              tags       ;(listof string?)
              blurb      ;string? - the post summary
              more?      ;boolean? - is `body` more than just `blurb`?
              body       ;string? - the post full contents
              ) #:prefab)

(define post-file-px
  #px"^(\\d{4}-\\d{2}-\\d{2})-(.+?)\\.(?:md|markdown|mdt|scrbl|html)$")
