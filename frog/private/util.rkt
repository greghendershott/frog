#lang racket/base

(require racket/contract
         racket/file
         racket/pretty
         rackjure/threading
         (only-in markdown display-xexpr)
         "verbosity.rkt")

(provide pp
         display-to-file*
         write-to-file*
         copy-file*
         make-directories-if-needed
         delete-file*
         delete-files*
         in-slice
         split-common-prefix
         path-get-extension
         exn->string)

;; Less typing, and also returns its value so good for sticking in
;; threading macros for debugging.
(define (pp v)
  (pretty-print v)
  v)

;; Like display-to-file, but makes directories if needed.
(define (display-to-file* v path #:exists exists #:mode [mode 'binary])
  (make-directories-if-needed path)
  (display-to-file v path #:exists exists #:mode mode))

;; Like write-to-file, but makes directories if needed.
(define (write-to-file* v path #:exists exists #:mode [mode 'binary])
  (make-directories-if-needed path)
  (write-to-file v path #:exists exists #:mode mode))

(define (copy-file* from to [exists-ok? #f])
  (make-directories-if-needed to)
  (copy-file from to exists-ok?))

(define (make-directories-if-needed path)
  (with-handlers ([exn:fail? void])
    (define-values (base name dir?)(split-path path))
    (make-directory* base)))

;; Like delete-file, but: (a) Doesn't raise exn when file doesn't
;; exist. (b) Does a prn1 "Deleted <path>" where the _displayed_ value
;; of path is transformed by an optional function.
(define (delete-file* path [display-path-as values])
  (when (file-exists? path)
    (delete-file path)
    (prn1 "Deleted ~a" (display-path-as path))))

(define (delete-files* dir [display-path-as values])
  (when (directory-exists? dir)
    (for ([file (directory-list dir)])
      (delete-file* (build-path dir file) display-path-as))))

;; For Rackets too old to have in-slice
(define (-in-slice k seq)
  (make-do-sequence
   (λ ()
     (define-values (more? get) (sequence-generate seq))
     (values
      (λ (_)
        (for/list ([i (in-range k)] #:when (more?))
          (get)))
      values
      #f
      #f
      (λ (val) (pair? val))
      #f))))

(define in-slice
  (with-handlers ([exn:fail? (λ _ -in-slice)])
    (dynamic-require 'racket/sequence 'in-slice)))

;; For Rackets too old to have split-common-prefix
(define/contract (-split-common-prefix as bs [same? equal?])
  (->* (list? list?) ((-> any/c any/c boolean?)) (values list? list? list?))
  (let loop ([as as] [bs bs])
    (if (and (pair? as) (pair? bs) (same? (car as) (car bs)))
        (let-values ([(prefix atail btail) (loop (cdr as) (cdr bs))])
          (values (cons (car as) prefix) atail btail))
        (values null as bs))))

(define split-common-prefix
  (with-handlers ([exn:fail? (λ _ -split-common-prefix)])
    (dynamic-require 'racket/list 'split-common-prefix)))

;; NOTE: these functions are copied from
;; https://github.com/racket/racket/blob/master/racket/collects/racket/path.rkt
;; once we decide to drop the support of versions below 6.5, delete them
;; and switch to use path-get-extension from racket/path instead
(define (file-name who name dir-ok?)
  (unless (or (path-string? name)
              (path-for-some-system? name))
    (raise-argument-error who "(or/c path-string? path-for-some-system?)" name))
  (let-values ([(base file dir?) (split-path name)])
    (and (or dir-ok? (not dir?))
         (path-for-some-system? file) file)))
(define (path-get-extension name)
  (let* ([name (file-name 'path-get-extension name #t)]
         [name (and name (path->bytes name))])
    (cond [(and name (regexp-match #rx#"(?<=.)([.][^.]+)$" name)) => cadr]
          [else #f])))


;; NOTE: these functions are copied from
;; https://github.com/racket/racket/blob/master/racket/collects/racket/exn.rkt
;; once we decide to drop the support of versions below 6.3, delete them
;; and switch to use exn->string from racket/exn instead
(define (exn->string exn)
  (if (exn? exn)
      (parameterize ([current-error-port (open-output-string)])
        ((error-display-handler) (exn-message exn) exn)
        (get-output-string (current-error-port)))
      (format "~s\n" exn)))

(module+ test
  (require rackunit)
  (define-syntax-rule (check-equal-values? generating-expr expected)
    (check-equal? (call-with-values (λ () generating-expr) list)
                  expected))
  (check-equal-values? (split-common-prefix '(a b c d) '(a b x y z))
                       '((a b)
                         (c d)
                         (x y z)))
  (check-equal-values? (split-common-prefix '() '())
                       '(() () ())))
