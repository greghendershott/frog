#lang racket/base

(require net/url
         racket/contract/base
         racket/contract/region
         racket/file
         racket/function
         racket/list
         racket/path
         racket/port
         racket/system
         racket/string
         (only-in racket/future processor-count)
         (only-in racket/match match-let-values)
         rackjure/threading
         rackjure/str
         "params.rkt"
         "util.rkt"
         "verbosity.rkt"
         "paths.rkt")

(provide make-responsive wait-resize-images clean-resized-images magick-available?)

(module+ test
  (require rackunit))

(define *max-jobs* (* 1.5 (processor-count)))  ; Arbitrary heuristic

;; Depend on ImageMagick
(define identify (find-executable-path "identify"))
(define mogrify (find-executable-path "mogrify"))

(define magick-available? (and identify mogrify))

(define (image-width path)
  (if (file-exists? path)
      (~> (with-output-to-string
            (λ ()
              (system* identify "-format" "%w" path)))
          string-trim
          string->number)
      (raise-argument-error 'image-width "Existing file" path)))

(module+ test
  (when magick-available?
    (parameterize ([top example]
                   [current-verbosity 99])
      (check-eq? (image-width (build-path (www/img-path) "800px-image.gif")) 800))))

(struct job (input out-path width) #:transparent)

(define (magick-args j)
  ;; Imagemagick options from
  ;; https://www.smashingmagazine.com/2015/06/efficient-image-resizing-with-
  ;;   imagemagick/
  `("-filter" "Triangle"
    "-define" "filter:support=2"
    "-unsharp" "0.25x0.08+8.3+0.045"
    "-dither" "None"
    "-posterize" "136"
    "-quality" "82"
    "-define" "jpeg:fancy-upsampling=off"
    "-define" "png:compression-filter=5"
    "-define" "png:compression-level=9"
    "-define" "png:compression-strategy=1"
    "-define" "png:exclude-chunk=all"
    "-interlace" "none"
    "-colorspace" "sRGB"
    "-thumbnail" ,(number->string (job-width j))
    "-path" ,(job-out-path j)
    ,(job-input j)))

(define master-worker
  (thread
   (λ ()
     (define (start-job j)
       (match-let-values ([(proc _ _ _) (apply subprocess
                                               (current-output-port)
                                               (current-input-port)
                                               (current-error-port)
                                               mogrify (magick-args j))])
         proc))
     ;; N.B: Config parameters set in the main thread are reset here
     ;; so make sure we do not rely on them. In particular prn1 and
     ;; prn2 will not output anything.
     (let ([finish #f]
           [mailbox (thread-receive-evt)])
       (let loop ([queue '()]
                  [procs '()])
         (let ([res (apply sync mailbox procs)])
           (cond
             [(subprocess? res)           ; Process terminated?
              (let ([status (subprocess-status res)])
                (unless (zero? status)
                  (eprintf "~a terminated with non-zero exit code: ~a\n"
                           mogrify status)))
              (let ([next-procs (remq res procs)])
                (if (not (empty? queue))
                    (begin
                      (let ([proc (start-job (first queue))])
                        (loop (rest queue) (cons proc next-procs))))
                    (unless (and (empty? next-procs) finish)
                      (loop queue next-procs))))]
             [(eq? res mailbox)
              (let ([msg (thread-receive)])
                (cond
                  [(eq? msg 'finish)
                   (set! finish #t)
                   (unless (empty? procs)
                     (displayln "Waiting for ImageMagick processes to finish.")
                     (loop queue procs))]
                  [(job? msg)
                   (let ([j msg])
                     (if (>= (length procs) *max-jobs*)
                         (loop (append queue (list j)) procs) ; FIFO queue semantics
                         (let ([proc (start-job j)])
                           (loop queue (cons proc procs)))))]))]
             [else
              (error "Unknown sync result: " res)
              (loop queue procs)])))))))

(define/contract (resize-image input new-width out-path)
  (path? number? path? . -> . void?)
  (prn1 "Shrinking ~a to ~a pixels asynchronously." (abs->rel/www input) new-width)
  ;; One problem with the async approach is that if Frog is killed before
  ;; subprocesses are finished they will not be triggered again if Frog is
  ;; invoked again and the source post has not been touched. Ideally we would
  ;; trap SIGINT and write out unfinished work to disk, or at least
  ;; detect that work was finished prematurely and clean and restart everything.
  (thread-send master-worker (job input out-path new-width)))

(define (wait-resize-images)
  (thread-send master-worker 'finish)
  (thread-wait master-worker))

(module+ test
  (when magick-available?
    (parameterize ([top example]
                   [current-verbosity 0])
      (define tmp (find-system-path 'temp-dir))
      (define output (build-path tmp "600px-image.gif"))
      (test-eq? "resize"
                (begin
                  (resize-image (build-path (www/img-path) "600px-image.gif") 10 tmp)
                  (wait-resize-images)
                  (image-width output))
                10)
      (delete-file* output))))

(define/contract (get-images image-path)
  (path? . -> . (values pair? (listof pair?)))
  (define resized-dir (build-path* (www/img-path) (current-image-output-dir)))
  (unless (directory-exists? resized-dir)
    (make-directory resized-dir))
  (let* ([orig-size (image-width image-path)]
         [sizes (filter ((curry >) orig-size) (current-image-sizes))])
    (values (cons image-path orig-size)
            (append (for/list ([width sizes])
                      (define output-dir (build-path* resized-dir
                                                      (number->string width)))
                      (define output (build-path output-dir
                                                 (file-name-from-path image-path)))
                      (unless (directory-exists? output-dir)
                        (make-directory output-dir))
                      (unless (and (file-exists? output)
                                   (< (file-or-directory-modify-seconds image-path)
                                      (file-or-directory-modify-seconds output)))
                        ;; TODO Spawn asynchronously to enable utilizing more cores
                        (resize-image image-path width output-dir))
                      (cons output width))
                  (if (< (length sizes) (length (current-image-sizes)))
                      (list (cons image-path orig-size))
                      '())))))

(define default-image-idx
  (for/or ([v  (current-image-sizes)]
           [ix (in-naturals)])
    (and (= v (current-image-default-size)) ix)))

(define/contract (make-responsive path sizes)
  (path-string? (or/c string? #f) . -> . (listof pair?))
  (define image-path (build-path (www-path) (path->relative-path path)))
  (define-values (orig srcset) (get-images image-path))
  (define src (abs->rel/www (car (if (> (length srcset) default-image-idx)
                                     (list-ref srcset default-image-idx)
                                     orig))))
  (define srcset-string
    (string-join
     (for/list ([srcdef srcset])
       (format "~a ~aw" (~> (car srcdef)
                            abs->rel/www string->path
                            uri-encode-path path->string)
               (cdr srcdef)))
     ", "))
  `([src ,src]
    [srcset ,srcset-string]
    ,(let ((orig-width (cdr orig)))
       `[sizes ,(or sizes
                    (current-image-sizes-attr)
                    (format "(max-width: ~apx) 100vw, ~apx"
                            orig-width orig-width))])))

(define/contract (clean-resized-images)
  (-> any)
  (let ([out-dir (build-path* (www/img-path) (current-image-output-dir))])
    (when (directory-exists? out-dir)
      (fold-files (λ (path type v)
                    (when (eq? type 'file)
                      (delete-file path)
                      (prn2 "Deleted ~a" (abs->rel/www path))))
                  '() out-dir #f)
      (for-each delete-directory (directory-list out-dir #:build? #t))
      (delete-directory out-dir))))
