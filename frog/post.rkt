#lang racket/base

(struct post (title      ;string?
              dest-path  ;path? - full pathname of local HTML file
              uri-path   ;string? - path portion of URI, with leading /
              date       ;string? - 8601 datetime format
              tags       ;(listof string?)
              blurb      ;(listof xexpr?) - the post summary
              more?      ;boolean? - is `body` more than just `blurb`?
              body       ;(listof xexpr?) - the full post xexprs, with
                         ;any <!-- more --> line removed, but NOT
                         ;syntax highlighted
              ))

(provide (struct-out post))
