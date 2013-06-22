#lang racket

(provide pp
         display-to-file*
         copy-file*
         make-directories-if-needed)

;; Less typing, but also returns its value so good for sticking in ~>
;; for debugging
(define (pp v)
  (pretty-print v)
  v)

;; Like display-to-file, but makes directories if needed.
(define (display-to-file* v path #:exists exists #:mode [mode 'binary])
  (make-directories-if-needed path)
  (display-to-file v path #:exists exists #:mode mode))

(define (copy-file* from to [exists-ok? #f])
  (make-directories-if-needed to)
  (copy-file from to exists-ok?))

(define (make-directories-if-needed path)
  (with-handlers ([exn:fail? (const (void))])
    (define-values (base name dir?)(split-path path))
    (make-directory* base)))
