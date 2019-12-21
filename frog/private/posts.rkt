#lang racket/base

(require racket/require
         markdown
         (multi-in racket (contract file format match port string))
         (only-in racket/list empty?)
         threading
         (only-in srfi/1 break)
         "../config/private/load.rkt"
         "author.rkt"
         "bodies-page.rkt"
         "html.rkt"
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "read-scribble.rkt"
         "serialize-posts.rkt"
         "template.rkt"
         (only-in "util.rkt" delete-file* display-to-file*)
         "verbosity.rkt"
         "xexpr2text.rkt")

(provide clean-post-output-files
         read-post
         write-post-page)

(module+ test
  (require rackunit
           racket/function))

;; NOTE: Since the user may manually plop HTML files anywhere in
;; (www-path), we can't just go around deleting those. Instead, we
;; need to iterate sources and delete only HTMLs corresponding to
;; those.
(define (clean-post-output-files)
  (define posts (deserialize-posts))
  (for ([post (in-list (hash-values posts))])
    (delete-file* (post-dest-path post) abs->rel/www)))

(define/contract (read-post path)
  (path? . -> . (or/c post? #f))
  ;; Read Markdown, Markdown Template, Scribble, or HTML(ish) file
  (let/ec return
    (define-values (path-to name __) (split-path path))
    (match-define (pregexp post-file-px (list _ dt nm)) (path->string name))
    (prn1 "Reading ~a" (abs->rel/src path))
    (define xs
      (match (path->string name)
        [(pregexp "\\.scrbl$")
         (define img-dest (build-path (www/img-path)
                                      "posts"
                                      (~a dt "-" nm)))
         (read-scribble-file path
                             #:img-local-path img-dest
                             #:img-uri-prefix (canonical-uri
                                               (abs->rel/www img-dest)))]
        [(pregexp "\\.html$")
         (define same.scrbl (path-replace-suffix path ".scrbl"))
         (when (file-exists? same.scrbl)
           (prn0 "Skipping ~a on the assumption it is a DrRacket preview output for ~a."
                 (abs->rel/src path)
                 (abs->rel/src same.scrbl))
           (return #f))
         (read-html-file path)]
        [(pregexp "\\.(?:md|markdown)$")
         ;; Footnote prefix is date & name w/o ext
         ;; e.g. "2010-01-02-a-name"
         (define footnote-prefix (~> (~a dt "-" nm) string->symbol))
         (parse-markdown path footnote-prefix)]
        [(pregexp "\\.mdt$")
         (define footnote-prefix (~> (~a dt "-" nm) string->symbol))
         (define text (render-template path-to (path->string name) '()))
         (parse-markdown text footnote-prefix)]))
    ;; Split to the meta-data and the body
    (match-define (list title date-str tags body) (meta-data xs name))
    (when (member "DRAFT" tags)
      (prn0 "Skipping ~a because it has the tag, 'DRAFT'"
            (abs->rel/src path))
      (return #f))
    ;; Split out the blurb (may be less than the entire body)
    (define-values (blurb more?) (above-the-fold body))
    ;; Make the destination HTML pathname
    (define date-struct (date-string->struct/user-error path date-str))
    (define dest-path
      (permalink-path date-struct
                      (~> title string-downcase slug)
                      (match (path->string name)
                        [(pregexp post-file-px (list _ _ s)) s])
                      (current-permalink)))
    (post title
          path
          (file-or-directory-modify-seconds path)
          dest-path
          (canonical-uri (post-path->link dest-path))
          date-str
          #f ;; older
          #f ;; newer
          tags
          (~> blurb enhance-body xexprs->string)
          more?
          (~> body enhance-body xexprs->string))))

(define (date-string->struct/user-error path s)
  (with-handlers
    ([exn:fail?
      (λ (e)
        (raise-user-error
         (~a (path->string path)
             ": Date metadata must be ISO-8601 format -- yyyy-mm-ddThr:mn:sc -- but is")
         s))])
    (date->date-struct s)))

;; (listof xexpr?) path? -> (list string? string? (listof string?) (listof xexpr?))
(define (meta-data xs path)
  (define (err x)
    (raise-user-error 'error "~a: Must start with metadata but ~a" path x))
  (define (warn x)
    (prn0 (format "~a: Ignoring unknown metadata: ~v" path x)))
  (match xs
    [`(,(or `(pre () (code () . ,metas)) ;Markdown
            `(pre () . ,metas)           ;Markdown
            `(pre . ,metas)              ;Markdown
            `(p () . ,metas))            ;Scribble
       . ,more)
     ;; We don't want HTML entities like &ndash;
     (define plain-text (string-join (map xexpr->markdown metas) ""))
     (define h
       (for/fold ([h (hash)])
                 ([s (string-split plain-text "\n")])
         (match s
           [(pregexp "^ *(.+?): *(.*?) *$" (list _ k v))
            #:when (member k '("Title" "Date" "Tags" "Authors"))
            (hash-set h k v)]
           [s (warn s) h])))
     (define (required k)
       (define (fail [pre ""])
         (err (format "missing ~a~v" pre k)))
       (match (string-trim (hash-ref h k fail))
         ["" (fail "non-blank ")]
         [v  v]))
     (define (optional k)
       (hash-ref h k ""))
     (list (required "Title")
           (required "Date" )
           (append (~>> (optional "Tags")
                        tag-string->tags)
                   (~>> (optional "Authors")
                        tag-string->tags
                        (map make-author-tag)))
           more)]
    [(cons x _) (err (~a "found:\n" (format "~v" x)))]
    [_ (err "none found")]))

(module+ test
  (define p (string->path "/path/to/file"))
  (test-case "Various HTML \"envelopes\" and HTML entities"
    (check-equal? (meta-data `((pre () (code () "Title: title\nDate: date\nTags: DRAFT\n"))) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags: DRAFT\n")) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((pre "Title: title\nDate: date\nTags: DRAFT\n")) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((p () "Title: title" ndash "hyphen \nDate: date\nTags: DRAFT\n")) p)
                  (list "title-hyphen" "date" '("DRAFT") '())))
  (test-case "Authors meta data is converted to prefixed tags"
    (check-equal? (meta-data `((p () "Title: title\nDate: date\nTags: DRAFT\nAuthors:Alice Baker,Charlie Dan\n")) p)
                  (list "title"
                        "date"
                        (list "DRAFT"
                              (make-author-tag "Alice Baker")
                              (make-author-tag "Charlie Dan"))
                        '())))
  (test-case "Handle spaces (or not) around key: and value"
    (check-equal? (meta-data `((pre "Title:title\nDate:date\nTags:DRAFT\n")) p)
                  (list "title" "date" '("DRAFT") '()))
    (check-equal? (meta-data `((pre " Title:  title  \n  Date: date \nTags: DRAFT  \n")) p)
                  (list "title" "date" '("DRAFT") '())))
  (test-case "Error raised for missing metadata"
    (check-exn #px"missing \"Title\""
               (thunk (meta-data '((pre "")) p)))
    (check-exn #px"missing \"Title\""
               (thunk (meta-data '((p () "")) p))))
  (test-case "Error raised for blank Title or Date -- https://github.com/greghendershott/frog/issues/213"
    (check-exn #px"missing non-blank \"Title\""
               (thunk (meta-data '((pre "Title: \n")) p)))
    (check-exn #px"missing non-blank \"Date\""
               (thunk (meta-data '((pre "Title: Some Title\nDate: \n")) p))))
  (test-case "https://github.com/greghendershott/frog/issues/142"
    (check-equal? (meta-data '((p
                                ()
                                "Title: A Beginner"
                                rsquo
                                "s Scribble Post\nDate: 2013-06-19T00:00:00\nTags: Racket, blogging"))
                             (string->path "/"))
                  (list "A Beginner's Scribble Post"
                        "2013-06-19T00:00:00"
                        '("Racket" "blogging")
                        '())))
  (test-case "https://github.com/greghendershott/frog/issues/189"
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags: \n")) p)
                  (list "title" "date" '() '()))
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags:\n")) p)
                  (list "title" "date" '() '()))
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\n")) p)
                  (list "title" "date" '() '())))
  (test-case "https://github.com/greghendershott/frog/issues/211"
    (check-equal? (meta-data `((pre () "Title: title\nDate: date\nTags:\n")) p)
                  (list "title" "date" '() '()))))

(define (tag-string->tags s)
  (match (regexp-split #px"," (string-trim s))
    ['("") '()]
    [ss    (map string-trim ss)]))

(module+ test
  (check-equal? (tag-string->tags "  ")
                '())
  (check-equal? (tag-string->tags " some, post ,   tags ")
                '("some" "post" "tags")))

(define (above-the-fold xs)
  (define-values (above below) (break more-xexpr? xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(p ,(pregexp "\\s*<!--\\s*more\\s*-->\\s*")) #t] ;old markdown parser
    [`(!HTML-COMMENT () ,(pregexp "more")) #t]         ;new markdown parser
    [_ #f]))

(module+ test
  (check-true (more-xexpr? `(p   "<!--more-->")))
  (check-true (more-xexpr? `(p " <!-- more -->")))
  (check-true (more-xexpr? `(p "<!--  more  -->")))
  (check-false (more-xexpr? "not more")))

(define (read-html-file path)
  (match (file->string path)
    [(pregexp "^(\\s{4}Title:.*?\n\\s{4}Date:.*?\n\\s{4}Tags:.*?\n+)\\s*(.*)$"
              (list _ md html))
     (append (parse-markdown md)
             (~>> (with-input-from-string html read-html-as-xexprs)
                  cddr))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (write-post-page p older newer)
  (post? (or/c post? #f) (or/c post? #f) . -> . void)
  (match-define (post title _ _ dest-path uri-path date _ _ tags blurb _ body) p)
  (prn1 "Generating post ~a" (abs->rel/www dest-path))
  (define older-uri (and older (post-uri-path older)))
  (define newer-uri (and newer (post-uri-path newer)))
  (~> (render-template
       (src-path)
       "post-template.html"
       (hasheq 'title       (title->htmlstr title)
               'uri-prefix  (or (current-uri-prefix) "")
               'uri-path    uri-path
               'full-uri    (full-uri uri-path)
               'date-8601   date
               'date-struct (date->date-struct date)
               'date        (~> date date->xexpr xexpr->string)
               'tags        (~> tags tags->xexpr xexpr->string)
               'authors     (~> tags author-tags->xexpr xexpr->string)
               'date+tags   (~> (date+tags->xexpr date tags) xexpr->string)
               'content     body
               'older-uri   older-uri
               'newer-uri   newer-uri
               'older-title (and older (title->htmlstr (post-title older)))
               'newer-title (and newer (title->htmlstr (post-title newer)))))
      (bodies->page #:title title
                    #:description (blurb->description blurb)
                    #:uri-path uri-path
                    #:keywords tags
                    #:rel-next older-uri
                    #:rel-prev newer-uri)
      (display-to-file* dest-path #:exists 'replace)))
