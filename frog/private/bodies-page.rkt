#lang racket/base

(require markdown
         net/uri-codec
         racket/dict
         racket/date
         (only-in racket/list add-between)
         racket/match
         racket/port
         racket/string
         rackjure/str
         rackjure/threading
         "author.rkt"
         "feeds.rkt"
         "html.rkt"
         "params.rkt"
         "paths.rkt"
         "template.rkt"
         "xexpr2text.rkt")

(provide all-tags
         bodies->page
         xexprs->string
         tags->xexpr
         author-tags->xexpr
         blurb->description
         title->htmlstr
         date->date-struct
         date+tags->xexpr
         date->xexpr)

(module+ test
  (require rackunit))

;; All tags, and the source file pathnames of the posts using them.
;; (hash/c string? (listof path?))
(define all-tags (make-parameter (make-hash)))

;; Put the body elements in a master page template.
(define (bodies->page contents                  ;string?
                      #:title title             ;string?
                      #:description description ;string?
                      #:uri-path uri-path       ;string?
                      #:feed [feed "all"]       ;string?
                      #:keywords [keywords '()] ;listof string?
                      #:tag [tag #f]            ;(or/c string? #f)
                      #:rel-prev [rel-prev #f]  ;(or/c string? #f)
                      #:rel-next [rel-next #f]) ;(or/c string? #f)
  (render-template
   (src-path)
   "page-template.html"
   (hasheq 'contents          contents
           'title             title
           'author            (current-author)
           'description       description
           'uri-prefix        (or (current-uri-prefix) "")
           'uri-path          uri-path
           'full-uri          (full-uri uri-path)
           'atom-feed-uri     (atom-feed-uri feed)
           'rss-feed-uri      (rss-feed-uri feed)
           'keywords          (string-join keywords ", ")
           'table-of-contents ""
           'tag               tag
           'rel-prev          rel-prev
           'rel-next          rel-next
           'all-tag-pairs     all-tag-pairs
           'tags-list-items   (xexprs->string (tags-list-items))
           'tags/feeds        (xexprs->string (tags/feeds)))))

(define (xexprs->string xs)
  (string-join (map xexpr->string xs) "\n"))

(define (tags/feeds)
  `((p "Tags:"
       (ul ,@(for/list ([(k v) (in-dict (tags-alist))])
               `(li ,(tag->xexpr k)
                    nbsp
                    ,@(if (current-show-tag-counts?) `(,(format "(~a)" v)) '())
                    " "
                    (a ([href ,(atom-feed-uri k)])
                       (img ([src ,(canonical-uri "/img/feed.png")]))))))
    (p (a ([href ,(current-posts-index-uri)]) "All Posts")
       " "
       (a ([href ,(atom-feed-uri "all")])
          (img ([src ,(canonical-uri "/img/feed.png")])))))))

(define (all-tag-pairs)
  (map tag->pair (dict-keys (tags-alist))))


(define (tags-list-items)
  (for/list ([(k v) (in-dict (tags-alist))])
               `(li ,(tag->xexpr k))))

(define (tags-alist)
  ;; Sort alphabetically by tag name. Use association list (can't sort
  ;; a hash).
  (~> (for/list ([(k v) (in-hash (all-tags))]
                 #:unless (member k '("all" "UNLINKED")))
        (cons k v))
      (sort string-ci<=? #:key car)))

(define (tags->xexpr tags)
  `(span
    ([class "tags"])
    ,@(add-between (for/list ([t (in-list tags)]
                              #:when (and (not (equal? t "UNLINKED"))
                                          (not (author-tag t))))
                     (tag->xexpr t values))
                   ", ")))

(define (author-tags->xexpr tags)
  `(span
    ([class "authors"])
    ,@(match (add-between (for/list ([t (in-list tags)]
                                     #:when (and (not (equal? t "UNLINKED"))
                                                 (author-tag t)))
                            (tag->xexpr t author-tag))
                          ", ")
        [(list) (list (current-author))]
        [as     as])))

(module+ test
  (define tags (list "foo" "bar"
                     (make-author-tag "Alice Baker")
                     (make-author-tag "Charlie Dean")))
  (test-case "tags->xexpr produces xexpr for non-author tags"
    (check-equal? (tags->xexpr tags)
                  `(span
                    ((class "tags"))
                    (a ((href "/tags/foo.html")) "foo")
                    ", "
                    (a ((href "/tags/bar.html")) "bar"))))
  (test-case "author-tags->xexpr produces xexpr for author tags"
    (check-equal? (author-tags->xexpr tags)
                  `(span
                    ((class "authors"))
                    (a ((href "/tags/Author-Alice-Baker.html")) "Alice Baker")
                    ", "
                    (a ((href "/tags/Author-Charlie-Dean.html")) "Charlie Dean"))))
  (test-case "No author tags results in default author"
    (parameterize ([current-author "Default"])
      (check-equal? (author-tags->xexpr '())
                    `(span
                      ((class "authors"))
                      ,(current-author))))))

(define (tag->pair s [display values])
  `(,(display s) . ,(canonical-uri (str "/tags/" (slug s) ".html"))))

(define (tag->xexpr s [display values])
  `(a ([href ,(canonical-uri (str "/tags/" (slug s) ".html"))])
      ,(display s)))

(define (blurb->description s)
  (~> (with-input-from-string s read-html-as-xexprs)
      xexprs->description))

(define (title->htmlstr t)
  ;; `parse-markdown` returns (listof xexpr?). For simple "one-liner"
  ;; markdown that's usually a list with just a single 'p element. In
  ;; that case, discard the 'p and use its body element(s). If it
  ;; parsed to something more complicated, the visual result will
  ;; probably be unappealing, but at least handle that case here.
  (define xs (match (parse-markdown t)
               [`((p () . ,xs)) xs]
               [xs xs]))
  (string-join (map xexpr->string xs) ""))

(define (date->date-struct YYYY-MM-DD-string)
  (match YYYY-MM-DD-string
    [(pregexp "(\\d{4})-(\\d{2})-(\\d{2})" (list _ y m d))
     (seconds->date (find-seconds 0 0 0
                                  (string->number d)
                                  (string->number m)
                                  (string->number y)))]))

(define (date+tags->xexpr date tags)
  `(p ([class "date-and-tags"])
      ,(date->xexpr date)
      " :: "
      ,(tags->xexpr tags)))

(define (date->xexpr date)
  (define dt (substring date 0 10)) ;; just YYYY-MM-DD
  `(time ([datetime ,dt]
          [pubdate "true"]) ,dt))
