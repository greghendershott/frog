#lang rackjure

(require markdown
         "bodies-page.rkt"
         "enhance-body.rkt"
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "scribble.rkt"
         "stale.rkt"
         "template.rkt"
         "util.rkt"
         "verbosity.rkt"
         "xexpr2text.rkt")

(provide clean-non-post-output-files
         build-non-post-pages)

;; NOTE: Since the user may manually plop HTML files anywhere in
;; (www-path), we can't just go around deleting those. Instead, we
;; need to iterate sources and delete only HTMLs corresponding to
;; those.
(define (clean-non-post-output-files)
  (define (maybe-delete path type v)
    (define-values (_ name __) (split-path path))
    (when (and (eq? type 'file)
               (regexp-match? #px"\\.(?:md|markdown|scrbl)$" path)
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
    (define-values (_ name __) (split-path path))
    (unless (and (eq? type 'file)
                 (regexp-match? #px"\\.(?:md|markdown|scrbl)$" path)
                 (not (regexp-match? post-file-px (path->string name))))
      (return v))
    (define dest-path (build-path (www-path)
                                  (~> path
                                      (path-replace-suffix ".html")
                                      abs->rel/src)))
    (define uri-path (abs->rel/www dest-path))
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
                                 #:img-uri-prefix (abs->rel/www img-dest))]
            [_ (parse-markdown path)])
          enhance-body))
    (define title
      (match xs
        ;; First h1 header, if any
        [(list-no-order `(h1 (,_ ...) ... ,els ...) _ ...)
         (string-join (map xexpr->markdown els) "")]
        ;; Else name of the source file
        [_ (~> path
               (path-replace-suffix "")
               file-name-from-path
               path->string)]))
    (prn1 "Generating non-post ~a" (abs->rel/www dest-path))
    (~> xs
        xexprs->string
        (bodies->page #:title title
                      #:description (xexprs->description xs)
                      #:uri-path uri-path)
        (display-to-file* dest-path #:exists 'replace))
    (cons uri-path v)))
