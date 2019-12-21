#lang racket/base

(require markdown
         racket/file
         racket/match
         racket/path
         racket/string
         threading
         "../config/private/load.rkt"
         "bodies-page.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "read-scribble.rkt"
         "stale.rkt"
         "template.rkt"
         (except-in "util.rkt" path-get-extension)
         "verbosity.rkt"
         "xexpr2text.rkt")

(provide clean-non-post-output-files
         build-non-post-pages)

(module+ test (require rackunit))

(define non-post-file-px #px"\\.(?:md|markdown|mdt|scrbl)$")

;; NOTE: Since the user may manually plop HTML files anywhere in
;; (www-path), we can't just go around deleting those. Instead, we
;; need to iterate sources and delete only HTMLs corresponding to
;; those.
(define (clean-non-post-output-files)
  (define (maybe-delete path type v)
    (define-values (_ name __) (split-path path))
    (when (and (eq? type 'file)
               (regexp-match? non-post-file-px path)
               (not (regexp-match? post-file-px (path->string name))))
      (define dest-path (build-path (www-path)
                                    (~> path
                                        (path-replace-suffix ".html")
                                        abs->rel/src)))
      (delete-file* dest-path abs->rel/www)))
  (fold-files maybe-delete '() (src-path) #f))

(define (build-non-post-pages) ;; -> (listof string?)
  (fold-files write-non-post-page '() (src-path) #f))

(define (write-non-post-page path type v)
  (let/ec return
    (define-values (path-to name __) (split-path path))
    (unless (and (eq? type 'file)
                 (regexp-match? non-post-file-px path)
                 (not (regexp-match? post-file-px (path->string name))))
      (return v))
    (define dest-path (build-path (www-path)
                                  (~> path
                                      (path-replace-suffix ".html")
                                      abs->rel/src)))
    (define uri-path (canonical-uri (abs->rel/www dest-path)))
    (unless (stale? dest-path path (page-template.html))
      (prn2 "Already up-to-date: ~a" dest-path)
      (return (cons uri-path v)))
    (prn1 "Reading non-post ~a" (abs->rel/src path))
    (define xs
      (~> (match (path->string name)
            [(pregexp "\\.scrbl$")
             (define img-dest (path-replace-suffix dest-path ""))
             (read-scribble-file path
                                 #:img-local-path img-dest
                                 #:img-uri-prefix (canonical-uri
                                                   (abs->rel/www img-dest)))]
            [(pregexp "\\.(?:md|markdown)$")
             (parse-markdown path)]
            [(pregexp "\\.mdt$")
             (define text (render-template path-to (path->string name) '()))
             (parse-markdown text)])
          enhance-body))
    (prn1 "Generating non-post ~a" (abs->rel/www dest-path))
    (~> xs
        xexprs->string
        (bodies->page #:title (make-title xs path)
                      #:description (xexprs->description xs)
                      #:uri-path uri-path)
        (display-to-file* dest-path #:exists 'replace))
    (cons uri-path v)))

(define (make-title xs path)
  (or (for/or ([x (in-list xs)])
        (match x
          ;; First h1 header, if any -- Scribble style with <a> anchor
          [`(h1 (,_ ...) (a . ,_) . ,els)
           (string-join (map xexpr->markdown els) "")]
          ;; First h1 header, if any -- otherwise
          [`(h1 (,_ ...) . ,els)
           (string-join (map xexpr->markdown els) "")]
          [_ #f]))
      ;; Else name of the source file
      (~> path
          (path-replace-suffix "")
          file-name-from-path
          path->string)))

(module+ test
  (check-equal?
   (make-title '((h1 () "The Title")
                 (h1 () "Not the title")
                 (p () "Blah blah"))
               #f)
   "The Title")
  (check-equal?
   (make-title
    '((h1 () (a ((name "(part._.The_.Title)"))) "The Title")
      (h1 () "1" (tt () nbsp) (a ((name "(part._.Section_1)"))) "Section 1")
      (p () "Here is some text."))
    #f)
   "The Title"))
