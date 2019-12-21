#lang racket/base

(require racket/require
         file/md5
         (multi-in racket (contract file function))
         threading)

(provide watch-directory
         checksum-path
         checksum-item)

(module+ test
  (require rackunit))

;; This is a poor person's version of OS-specific mechanisms like
;; FindFirstFileChangeNotification on Windows and fsevents on OS X.
;; Instead this does a checksum of a path's contents, running a thread
;; to check periodically.
;;
;; A version of this which _does_ use FFI to access OS-specific
;; services would be great.
;;
;; Example ussage:
;; (require racket/runtime-path)
;; (define-runtime-path p "/tmp/test")
;; (define t (watch-directory p
;;                            '(file)
;;                            (lambda (p what)
;;                              (printf "~a: ~a\n" p what))))
;; (printf "Watching path ~a. Try changing files there.\n" p)
;; (displayln "Enter QUIT to stop.")
;; (void (read))
;; (kill-thread t)

(define/contract (watch-directory path
                                  types
                                  on-change
                                  #:rate [rate 3])
  ((path?
    (listof (or/c 'file 'dir 'link))
    (path? (or/c 'create 'delete 'modify) . -> . any))
   (#:rate (and/c positive? number?))
   . ->* . thread?)
  (thread (thunk
           (let loop ([old (checksum-path path types)])
             (sleep rate)
             (define new (checksum-path path types))
             (for ([(k v) (in-hash new)])
               (cond [(not (hash-has-key? old k))
                      (on-change k 'create)]))
             (for ([(k v) (in-hash old)])
               (cond [(not (hash-has-key? new k))
                      (on-change k 'delete)]
                     [(not (equal? (hash-ref new k) v))
                      (on-change k 'modify)]))
             (loop new)))))

(define/contract (checksum-path path types)
  (path? (listof (or/c 'file 'dir 'link)) . -> . (hash/c path? bytes?))
  (fold-files (lambda (path type h)
                (cond [(memq type types)
                       (hash-set h path (checksum-item path))]
                      [else h]))
              (hash)
              path))

(define/contract (checksum-item path)
  (path? . -> . bytes?)
  (~> path
      path->uid
      number->string
      string->bytes/utf-8
      (md5 #f)))

(define/contract (path->uid path)
  (path? . -> . integer?)
  ;; Try to use the modification time. (Hashing the actual file
  ;; contents would be more reliable, but much more expensive, and
  ;; unnecessary for many applications.)
  ;;
  ;; If we can't get the modify time (e.g. permission error) then
  ;; simply hash the path name. (In that case we will be able to
  ;; report something created or deleted, but modified.)
  (define (path->integer path)
    (for/fold ([x 0])
              ([c (in-string (path->string path))])
      (+ x (char->integer c))))
  (with-handlers ([exn? (lambda (_) (path->integer path))])
    (file-or-directory-modify-seconds path)))
