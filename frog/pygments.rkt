#lang rackjure

(require racket/runtime-path
         xml
         (prefix-in h: html)
         "verbosity.rkt")

(provide start-pygments
         stop-pygments
         pygmentize)

(define-runtime-path pipe.py "pipe.py")
;;(define-runtime-path pipe.py "fart.py")

(define (start-pygments-process)
  (match-define (list pyg-in pyg-out pyg-pid pyg-err pyg-proc)
                (process (str "python -u " pipe.py)))
  (sleep 1)
  (define (running?)
    (eq? 'running (pyg-proc 'status)))
  (case-lambda
    [(code lang)
     (cond [(running?)
            (file-stream-buffer-mode pyg-out 'line)
            (displayln lang pyg-out)
            (displayln code pyg-out)
            (displayln "__END__" pyg-out)
            (let loop ([s ""])
              (match (read-line pyg-in 'any)
                ["__END__" (~> (open-input-string s)
                               (h:read-html-as-xml)
                               ((lambda (xs) (make-element #f #f '*root '() xs)))
                               xml->xexpr
                               cddr)]
                [(? string? v) (loop (str s v "\n"))]
                [_ (copy-port pyg-err (current-output-port))]))]
           [else code])]
    [() ;; stop
     (cond [(running?)
            (file-stream-buffer-mode pyg-out 'line)
            (displayln "__EXIT__" pyg-out)
            (begin0 (or (pyg-proc 'exit-code) (pyg-proc 'kill))
              (close-input-port pyg-in)
              (close-output-port pyg-out)
              (close-input-port pyg-err))]
           [else 0])]))

(define pyg #f)

(define (start-pygments)
  (prn2 "Trying to start pygments process")
  (set! pyg (start-pygments-process)))

(define (stop-pygments)
  (prn2 "Stopping pygments process")
  (pyg)
  (set! pyg #f))

(define (pygmentize code lang)
  (cond [pyg (pyg code lang)]
        [else code]))

;; (start-pygments)
;; (define-runtime-path pygments.rkt "pygments.rkt")
;; (pretty-print (pygmentize (file->string pygments.rkt) "racket"))
;; (stop-pygments)
;; ;; How does it handle this?
;; (pretty-print (pygmentize (file->string pygments.rkt) "racket"))
