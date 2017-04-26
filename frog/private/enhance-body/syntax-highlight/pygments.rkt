#lang rackjure/base

(require racket/function
         racket/match
         racket/port
         racket/runtime-path
         racket/system
         rackjure/str
         rackjure/conditionals
         "../../html.rkt"
         "../../verbosity.rkt")

(provide pygmentize)

(module+ test
  (require rackunit))

;; Process that runs Python with our pipe.py script.

(define-values (pyg-in pyg-out pyg-pid pyg-err pyg-proc)
  (values #f #f #f #f #f))
(define-runtime-path pipe.py "pipe.py")

(define start
  (let ([start-attempted? #f])
    (λ (python-executable line-numbers? css-class)
      (unless start-attempted?
        (set! start-attempted? #t)
        (if-let [python (find-executable-path python-executable)]
          (begin
            (prn0 (str "Launching " python " pipe.py"))
            (match (process
                    (str python " -u " pipe.py
                         (if line-numbers? " --linenos" "")
                         " --cssclass " css-class))
              [(list in out pid err proc)
               (set!-values (pyg-in pyg-out pyg-pid pyg-err pyg-proc)
                            (values in out pid err proc))
               (file-stream-buffer-mode out 'line)
               (match (read-line pyg-in 'any)  ;; consume "ready" line or EOF
                 [(? eof-object?) (say-no-pygments)]
                 [_ (say-pygments)])]
              [_ (say-no-pygments)]))
          (say-no-pygments))))))

(define (say-pygments)
  (prn1 "Using Pygments."))
(define (say-no-pygments)
  (prn1 "Pygments not found. Using plain `pre` blocks."))

(define (running?)
  (and pyg-proc
       (eq? (pyg-proc 'status) 'running)))

(define (stop) ;; -> void
  (when (running?)
    (displayln "__EXIT__" pyg-out)
    (begin0 (or (pyg-proc 'exit-code) (pyg-proc 'kill))
      (close-input-port pyg-in)
      (close-output-port pyg-out)
      (close-input-port pyg-err)))
  (void))

(exit-handler
 (let ([old-exit-handler (exit-handler)])
   (lambda (v)
     (stop)
     (old-exit-handler v))))

(define (pygmentize code lang
                    #:python-executable python-executable
                    #:line-numbers? line-numbers?
                    #:css-class css-class)
  (define (default code)
    `((pre () (code () ,code))))
  (unless (running?)
    (start python-executable line-numbers? css-class))
  (cond [(running?)
         (displayln lang pyg-out)
         (displayln code pyg-out)
         (displayln "__END__" pyg-out)
         (let loop ([s ""])
           (match (read-line pyg-in 'any)
             ["__END__" (with-input-from-string s read-html-as-xexprs)]
             [(? string? v) (loop (str s v "\n"))]
             [_ (copy-port pyg-err (current-output-port)) ;echo error msg
                (default code)]))]
        [else (default code)]))
