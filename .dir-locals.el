((nil
  (indent-tabs-mode         . nil)
  (require-final-newline    . t)
  (show-trailing-whitespace . t))
 (prog-mode
  (comment-column . 40)
  (fill-column    . 70))
 (makefile-mode
  (indent-tabs-mode . t))
 (racket-mode
  ;; Better indentation for quoted xexprs and for at-exprs:
  (racket-indent-sequence-depth    . 3)
  (racket-indent-curly-as-sequence . t)))
