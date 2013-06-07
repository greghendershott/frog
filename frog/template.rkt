#lang racket/base

(require racket/dict web-server/templates)

(provide render-template)

(define (render-template dir filename dict)
  (define namespace-for-template (make-empty-namespace))
  (namespace-attach-module (current-namespace)
                           'web-server/templates
                           namespace-for-template)
  (for ([(k v) (in-dict dict)])
    (namespace-set-variable-value! k v #f namespace-for-template))
  (parameterize [(current-namespace namespace-for-template)]
    (namespace-require 'web-server/templates))
  (define to-eval
    #`(include-template #,(datum->syntax #'render-template filename)))
  (parameterize ([current-directory dir])
    (eval to-eval namespace-for-template)))
