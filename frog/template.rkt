#lang racket/base

(require web-server/templates)

(provide render-template)

(define (render-template dir filename dict)
  (define namespace-for-template (make-empty-namespace))
  (namespace-attach-module (current-namespace) 'web-server/templates
                           namespace-for-template)
  (hash-map dict
            (lambda (key value)
              (namespace-set-variable-value!
               key
               value
               #f
               namespace-for-template)))
  (parameterize [(current-namespace namespace-for-template)]
    (namespace-require 'web-server/templates))
  (define to-eval
    #`(include-template #,(datum->syntax #'render-template filename)))
  (parameterize ([current-directory dir])
    (eval to-eval namespace-for-template)))
