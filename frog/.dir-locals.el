;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode)
  (require-final-newline . t)
  (show-trailing-whitespace . t))
 (makefile-mode
  (indent-tabs-mode . t))
 (racket-mode
  (comment-column . 40)
  (fill-column . 78))
 (scheme-mode
  (compile-command . "racket frog.rkt")))
