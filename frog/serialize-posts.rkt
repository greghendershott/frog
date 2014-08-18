#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/file
         racket/serialize
         "paths.rkt"
         "post-struct.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (clean-serialized-posts)
  (define (maybe-delete path type v)
    (when (eq? type 'file)
      (delete-file* path abs->rel/top)))
  (fold-files maybe-delete '() (obj-path) #f))

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
