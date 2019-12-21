#lang racket/base

(require racket/require
         (multi-in racket (list match string))
         "params.rkt"
         "post-struct.rkt")

(provide make-author-tag
         author-tag
         post-authors
         post-author)

(module+ test
  (require rackunit))

(define author-tag-prefix "Author: ")
(define px (string-append "^" author-tag-prefix "(.*)$"))

(define (make-author-tag s)
  (string-append author-tag-prefix s))

;; If an author tag return the author name, else #f
(define (author-tag s) ;String -> (Option String)
  (match s
    [(pregexp px (list _ author)) author]
    [_ #f]))

(module+ test
  (check-equal? (author-tag (make-author-tag "John Doe"))
                "John Doe")
  (check-false (author-tag "John Doe")))

(define (post-authors post) ;Post -> (Listof String)
  (match (filter-map author-tag (post-tags post))
    ['() (list (current-author))]
    [as  as]))

;; Like post-authors, but return a single string. If there are multiple
;; authors they are concatenated separated by commas.
(define (post-author post) ;Post -> String
  (string-join (post-authors post) ", "))

(module+ test
  (parameterize ([current-author "Default Author"])
   (test-case "Post with some byline tags"
     (let ([p (post "title" "src-path" 0 "dest-path" "uri-path" "date"
                    '() '()
                    (list "foo"
                          (make-author-tag "Alice Doe")
                          (make-author-tag "Bob Doe")
                          "bar"
                          "baz")
                    "blurb"
                    #f
                    "body")])
       (check-equal? (post-authors p)
                     (list "Alice Doe" "Bob Doe"))
       (check-equal? (post-author p)
                     "Alice Doe, Bob Doe")))
   (test-case "Post with no byline tags"
     (let ([p (post "title" "src-path" 0 "dest-path" "uri-path" "date"
                    '() '()
                    '("foo" "bar" "baz") ;no bylines
                    "blurb"
                    #f
                    "body")])
       (check-equal? (post-authors p)
                     (list "Default Author"))
       (check-equal? (post-author p)
                     "Default Author")))))
