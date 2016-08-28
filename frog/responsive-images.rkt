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
         rackjure/threading
         "params.rkt"
         "util.rkt"
         "verbosity.rkt"
         "paths.rkt")

(provide make-responsive clean-resized-images magick-available?)

;; Depend on ImageMagick
(define identify (find-executable-path "identify"))
(define mogrify (find-executable-path "mogrify"))

(define magick-available? (and identify mogrify))

(module+ test
  (require rackunit))

(define (image-width path)
  (~> (with-output-to-string
        (λ ()
          (system* identify "-format" "%w" path)))
      string-trim
      string->number))

(module+ test
  (when magick-available?
    (parameterize ([top example])
      (check-eq? (image-width (build-path (www/img-path) "800px-image.gif")) 800))))

(define/contract (resize-image in new-width out-path)
  (path? number? path? . -> . boolean?)
  (prn1 "Shrinking ~a to ~a pixels... " (abs->rel/www in) new-width)
  ;; Imagemagick options from
  ;; https://www.smashingmagazine.com/2015/06/efficient-image-resizing-with-imagemagick/
  (apply system* mogrify
         `("-filter" "Triangle" "-define" "filter:support=2"
           "-unsharp" "0.25x0.08+8.3+0.045" "-dither" "None" "-posterize" "136"
           "-quality" "82" "-define" "jpeg:fancy-upsampling=off"
           "-define" "png:compression-filter=5" "-define" "png:compression-level=9"
           "-define" "png:compression-strategy=1" "-define" "png:exclude-chunk=all"
           "-interlace" "none" "-colorspace" "sRGB"
           "-thumbnail" ,(number->string new-width)
           "-path" ,out-path ,in)))

(module+ test
  (when magick-available?
    (parameterize ([top example]
                   [current-verbosity 0])
      (define tmp (find-system-path 'temp-dir))
      (define output (build-path tmp "600px-image.gif"))
      (test-eq? "resize"
                (begin
                  (resize-image (build-path (www/img-path) "600px-image.gif") 10 tmp)
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
     (for/list ([srcdef  srcset])
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
