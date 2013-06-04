#lang racket

;; The first time it is called for `path`, `cached-file` reads the
;; file at `path` using the #:read function, and passing the result
;; through `post-proc` before returning it. If the file doesn't exist,
;; then the result of calling `default-proc` is returned.
;;
;; In addition, the `post-proc` return value is cached, and returned
;; on subsequent calls provided (a) the file still exists and (b) the
;; modification time has not changed. But if (a) the modification time
;; has changed, then the old value is flushed from cache and
;; `cached-file` calls itself as if for the first time. Or if (b) the
;; file no longer exists, then the old value is flushed from cache and
;; the result of calling `default-proc` is returned.

(provide cached-file)

(define h (make-hash)) ;(hash/c path? item?)
(struct item (v        ;any/c
              mod))    ;exact-integer?

(define/contract (cached-file path
                              #:read [reader port->string]
                              #:post-proc [post-proc identity]
                              #:default-proc [default-proc (const #f)])
  ((path?)
   (#:read (-> any/c)
    #:post-proc (any/c . -> . any/c)
    #:default-proc (-> any/c))
   . ->* . any/c)
  ;; Already in cache, or not?
  (cond [(hash-has-key? h path)
         ;; File still exists, or not?
         (cond [(file-exists? path)
                (match-define (item v mod) (hash-ref h path))
                (define mod-prev (file-or-directory-modify-seconds path))
                ;; Modification time the same, or not?
                (cond [(= mod mod-prev) (item-v (hash-ref h path))]
                      [else (hash-remove! h path)
                            (cached-file path reader post-proc default-proc)])]
               [else (hash-remove! h path)
                     (default-proc)])]
        ;; File exists, or not?
        [else (cond [(file-exists? path)
                     (define mod (file-or-directory-modify-seconds path))
                     (define v (post-proc (with-input-from-file path reader)))
                     (hash-set! h path (item v mod))
                     v]
                    [else (default-proc)])]))
