#lang rackjure/base

(require markdown
         racket/dict
         racket/list
         racket/match
         racket/port
         racket/string
         rackjure/str
         rackjure/threading
         "feeds.rkt"
         "html.rkt"
         "params.rkt"
         "paths.rkt"
         "template.rkt"
         "util.rkt"
         "xexpr2text.rkt")

(provide all-tags
         bodies->page
         xexprs->string
         tags->xexpr
         tag->xexpr
         blurb->description
         title->htmlstr
         date->date-struct
         date+tags->xexpr
         date->xexpr)

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
   {'contents contents
    'title title
    'author (current-author)
    'description description
    'uri-prefix (let ([prefix (current-uri-prefix)]) (if prefix prefix ""))
    'uri-path uri-path
    'full-uri (full-uri uri-path)
    'atom-feed-uri (atom-feed-uri feed)
    'rss-feed-uri (rss-feed-uri feed)
    'keywords (string-join keywords ", ")
    'table-of-contents ""
    'tag tag
    'rel-prev rel-prev
    'rel-next rel-next
    'tags-list-items (xexprs->string (tags-list-items))
    'tags/feeds (xexprs->string (tags/feeds))}))

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
                       (img ([src ,(canonicalize-uri "/img/feed.png")]))))))
    (p (a ([href ,(current-posts-index-uri)]) "All Posts")
       " "
       (a ([href ,(atom-feed-uri "all")])
          (img ([src ,(canonicalize-uri "/img/feed.png")])))))))

(define (tags-list-items)
  (for/list ([(k v) (in-dict (tags-alist))])
               `(li ,(tag->xexpr k))))

(define (tags-alist)
  ;; Sort alphabetically by tag name. Use association list (can't sort
  ;; a hash).
  (~> (for/list ([(k v) (in-hash (all-tags))]
                 #:unless (equal? k "all"))
        (cons k v))
      (sort string-ci<=? #:key car)))

(define (tags->xexpr tags)
  `(span ([class "tags"])
         ,@(add-between (map tag->xexpr tags)
                        ", ")))

(define (tag->xexpr s)
  `(a ([href ,(canonicalize-uri (str "/tags/" (our-encode s) ".html"))]) ,s))

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
     (date 0 0 0
           (string->number d) (string->number m) (string->number y)
           0 0 #f 0)]))

(define (date+tags->xexpr date tags)
  `(p ([class "date-and-tags"])
      ,(date->xexpr date)
      " :: "
      ,(tags->xexpr tags)))

(define (date->xexpr date)
  (define dt (substring date 0 10)) ;; just YYYY-MM-DD
  `(time ([datetime ,dt]
          [pubdate "true"]) ,dt))
