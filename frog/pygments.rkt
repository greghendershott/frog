#lang rackjure

(require racket/runtime-path
         xml
         (prefix-in h: html)
         "verbosity.rkt")

(provide start-pygments
         stop-pygments
         pygmentize)

(define-runtime-path pipe.py "pipe.py")

(define (start-pygments-process)
  ;; Start a subprocess running our pipe.py script:
  (match-define (list pyg-in pyg-out pyg-pid pyg-err pyg-proc)
                (process (str "python -u " pipe.py)))
  ;; Wait a second for process to start and load pipe.py, and either
  ;; be OK or error due to `import pygments` failing b/c Pygments not
  ;; installed.
  (sleep 1)
  (define (running?)
    (eq? 'running (pyg-proc 'status)))
  (unless (running?)
    (prn0 "Pygments not installed. Using plain `pre` blocks."))
  (define (lex code lang) ;; string? string? -> (listof xexpr?)
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
               [_ (copy-port pyg-err (current-output-port)) ;echo error msg
                  `((pre ,code))]))]
          [else `((pre ,code))]))
  (define (stop) ;; -> number?
    (cond [(running?)
           (file-stream-buffer-mode pyg-out 'line)
           (displayln "__EXIT__" pyg-out)
           (begin0 (or (pyg-proc 'exit-code) (pyg-proc 'kill))
             (close-input-port pyg-in)
             (close-output-port pyg-out)
             (close-input-port pyg-err))]
          [else 0]))
  (values lex stop))

(define pyg-lex #f)
(define pyg-stop #f)

(define (start-pygments)
  (prn2 "Trying to start pygments process")
  (set!-values (pyg-lex pyg-stop) (start-pygments-process)))

(define (stop-pygments)
  (prn2 "Stopping pygments process")
  (pyg-stop)
  (set!-values (pyg-lex pyg-stop) (values #f #f)))

(define (pygmentize code lang) ;; string? string? -> (listof xexpr?)
  (cond [pyg-lex (pyg-lex code lang)]
        [else `((pre ,code))]))
