#lang rackjure/base

(require racket/file
         racket/function
         racket/pretty
         rackjure/threading
         (only-in markdown display-xexpr)
         "verbosity.rkt")

(provide (all-defined-out))

;; Less typing, but also returns its value so good for sticking in ~>
;; for debugging
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
  (with-handlers ([exn:fail? (const (void))])
    (define-values (base name dir?)(split-path path))
    (make-directory* base)))

;; Like delete-file, but (a) doesn't abend if file doesn't exit, and
;; (b) does a prn1 "Deleted X" message.
(define (delete-file* path [f values])
  (when (file-exists? path)
    (delete-file path)
    (prn1 "Deleted ~a" (f path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (our-encode s)
  ;; Extremely conservative.
  ;;
  ;; WARNING: Changing this will break blog post permalink pattens that
  ;; use the {title} variable. Even if this could be improved, doing so
  ;; would break backward compatability.
  (~> (list->string (for/list ([c (in-string s)])
                      (cond [(or (char-alphabetic? c)
                                 (char-numeric? c)) c]
                            [else #\-])))
      (re* #px"-{2,}" "-")              ;only one hyphen in a row
      (re #px"-{1,}$" "")))             ;no hyphen at end

(define (re* s rx new)
  (regexp-replace* rx s new))
(define (re s rx new)
  (regexp-replace rx s new))

(module+ test
  (require rackunit)
  (check-equal? (our-encode "Foo? Bar. Baz.")
                "Foo-Bar-Baz")
  (check-equal? (our-encode "Here's a question--how many hyphens???")
                "Here-s-a-question-how-many-hyphens"))

