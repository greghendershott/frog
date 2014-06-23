#lang rackjure

(require markdown
         (only-in srfi/1 break)
         "bodies-page.rkt"
         "enhance-body.rkt"
         "html.rkt"
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "scribble.rkt"
         "serialize-posts.rkt"
         "template.rkt"
         "util.rkt"
         "xexpr2text.rkt"
         "verbosity.rkt")

(provide clean-post-output-files
         read-post
         write-post-page)

(module+ test (require rackunit))

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
  ;; Read Markdown, Scribble, or HTML(ish) file
  (let/ec return
    (define-values (_ name __) (split-path path))
    (match-define (pregexp post-file-px (list _ dt nm)) (path->string name))
    (prn1 "Reading ~a" (abs->rel/src path))
    (define xs
      (match (path->string name)
        [(pregexp "\\.scrbl$")
         (define img-dest (build-path (www/img-path)
                                      "posts"
                                      (str dt "-" nm)))
         (read-scribble-file path
                             #:img-local-path img-dest
                             #:img-uri-prefix (abs->rel/www img-dest))]
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
         (define footnote-prefix (~> (str dt "-" nm) string->symbol))
         (parse-markdown path footnote-prefix)]))
    ;; Split to the meta-data and the body
    (define-values (title date tags body) (meta-data xs name))
    (when (member "DRAFT" tags)
      (prn0 "Skipping ~a because it has the tag, 'DRAFT'"
            (abs->rel/src path))
      (return #f))
    ;; Split out the blurb (may be less than the entire body)
    (define-values (blurb more?) (above-the-fold body))
    ;; Make the destination HTML pathname
    (define year (substring date 0 4))
    (define month (substring date 5 7))
    (define day (substring date 8 10))
    (define dest-path
      (permalink-path year month day
                      (~> title string-downcase our-encode)
                      (match (path->string name)
                        [(pregexp post-file-px (list _ _ s)) s])
                      (current-permalink)))
    (post title
          path
          (file-or-directory-modify-seconds path)
          dest-path
          (post-path->link dest-path)
          date
          #f ;; older
          #f ;; newer
          tags
          (~> blurb enhance-body xexprs->string)
          more?
          (~> body enhance-body xexprs->string))))

;; (listof xexpr?) path? -> (values string? string? string? (listof xexpr?))
(define (meta-data xs path)
  (define (err x)
    (raise-user-error
     'error
     "Post file ~a.\nMust start with Title/Date/Tags, but found:\n~v"
     path
     x))
  (match xs
    [`(,(or `(pre () (code () . ,metas)) ;Markdown
            `(pre () . ,metas)           ;Markdown
            `(pre . ,metas)              ;Markdown
            `(p () . ,metas))            ;Scribble
       . ,more)
     ;; In the meta-data we don't want HTML entities like &ndash; we
     ;; want plain text.
     (match (string-join (map xexpr->markdown metas))
       [(pregexp "^Title:\\s*(.+?)\nDate:\\s*(.+?)\nTags:\\s*(.*?)\n*$"
                 (list _ title date tags))
        (values title date (tag-string->tags tags) more)]
       [_ (err (first xs))])]
    [(cons x _) (err x)]
    [_ (err "")]))

(module+ test
  (define p (string->path "/"))
  (check-not-exn (thunk (meta-data `((pre () (code () "Title: title\nDate: date\nTags: DRAFT\n"))) p)))
  (check-not-exn (thunk (meta-data `((pre () "Title: title\nDate: date\nTags: DRAFT\n")) p)))
  (check-not-exn (thunk (meta-data `((pre "Title: title\nDate: date\nTags: DRAFT\n")) p)))
  (check-not-exn (thunk (meta-data `((p () "Title: title" ndash "hyphen \nDate: date\nTags: DRAFT\n\n")) p)))
  (check-exn exn? (thunk (meta-data '((pre "not meta data")) p)))
  (check-exn exn? (thunk (meta-data '((p () "not meta data")) p))))

(define (tag-string->tags s)
  (~>> (regexp-split #px"," s)
       (map string-trim)))

(module+ test
  (check-equal? (tag-string->tags " some, post ,   tags ")
                '("some" "post" "tags")))

(define (above-the-fold xs)
  (define-values (above below) (break more-xexpr? xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(p ,(pregexp "\\s*<!--\\s*more\\s*-->\\s*")) #t]             ;Markdown
    [`(!HTML-COMMENT () ,(pregexp "more")) #t]                     ;Markdown
    [`(p () "<" "!" ndash ,(pregexp "\\s*more\\s*") ndash ">") #t] ;Scribble
    [_ #f]))

(module+ test
  (check-true (more-xexpr? `(p   "<!--more-->")))
  (check-true (more-xexpr? `(p " <!-- more -->")))
  (check-true (more-xexpr? `(p "<!--  more  -->")))
  (check-true (more-xexpr? `(p () "<" "!" ndash   "more"   ndash ">")))
  (check-true (more-xexpr? `(p () "<" "!" ndash  " more "  ndash ">")))
  (check-true (more-xexpr? `(p () "<" "!" ndash "  more  " ndash ">")))
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
       {'title (title->htmlstr title)
        'uri-path uri-path
        'full-uri (full-uri uri-path)
        'date-8601 date
        'date-struct (date->date-struct date)
        'date (~> date date->xexpr xexpr->string)
        'tags (~> tags tags->xexpr xexpr->string)
        'date+tags (~> (date+tags->xexpr date tags) xexpr->string)
        'content body
        'older-uri older-uri
        'newer-uri newer-uri
        'older-title (and older (title->htmlstr (post-title older)))
        'newer-title (and newer (title->htmlstr (post-title newer)))})
      (bodies->page #:title title
                    #:description (blurb->description blurb)
                    #:uri-path uri-path
                    #:keywords tags
                    #:rel-next older-uri
                    #:rel-prev newer-uri)
      (display-to-file* dest-path #:exists 'replace)))
