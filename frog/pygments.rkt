#lang rackjure

(require racket/runtime-path
         "params.rkt"
         "html.rkt"
         "verbosity.rkt")

(provide pygmentize)

;; Go ahead during module load and start a thread to launch the
;; subprocess that runs Python with our pipe.py script.
(define-runtime-path pipe.py "pipe.py")
(define-values (pyg-in pyg-out pyg-pid pyg-err pyg-proc)
  (values #f #f #f #f #f))

(define (make-pygments-thread)
  (thread
   (thunk
    ;; Start a subprocess running our pipe.py script.
    (match (process (str "python -u " pipe.py
                         (if (current-pygments-linenos?) " --linenos" "")
                         " --cssclass " (current-pygments-cssclass)))
      [(list in out pid err proc)
       (set!-values (pyg-in pyg-out pyg-pid pyg-err pyg-proc)
                    (values in out pid err proc))
       (file-stream-buffer-mode out 'line)])
    ;; Wait for its "ready\n"
    (when (sync/timeout 3 pyg-in)
      (read-line pyg-in 'linefeed)))))

(define load-thread #t)

(define (running?)
  (define (?)
    (and pyg-proc
         (eq? (pyg-proc 'status) 'running)))
  (when load-thread ;; first time
    (thread-wait (make-pygments-thread))
    (set! load-thread #f)
    (unless (?)
      (prn1 "Pygments not installed. Using plain `pre` blocks.")))
  (?))

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

(define (pygmentize code lang) ;; string? string? -> (listof xexpr?)
  (define (default code)
    `((pre () (code () ,code))))
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
