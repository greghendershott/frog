#lang racket/base

(require racket/require
         (multi-in racket (contract file serialize))
         "paths.rkt"
         "post-struct.rkt"
         "util.rkt")

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (clean-serialized-posts)
  (delete-files* (obj-path) abs->rel/top))

(define/contract (deserialize-posts)
  (-> (hash/c path? post?))
  (define path (build-path (obj-path) "build"))
  (or (and (file-exists? path)
           (deserialize (call-with-input-file* path read)))
      (make-hash)))

(define/contract (serialize-posts posts)
  (-> (hash/c path? post?) void)
  (define path (build-path (obj-path) "build"))
  (write-to-file* (serialize posts) path #:exists 'replace))
