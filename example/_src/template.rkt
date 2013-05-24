#lang rackjure

(provide container)

(define (container dict)
  ;; Remember that Twitter Bootstrap has a 12 column model. The spanN
  ;; should add up to 12.  For instance 3 "span4" divs, or 2 "span6"
  ;; divs, and so on. In this example we have a "span9" for the main
  ;; content and a "span3" sidebar to its right.
  ;;
  ;; The (dict key) expressions below are a #lang rackjure feature,
  ;; applicable dicts. They'e equivalent to (dict-ref dict key), which
  ;; you may use instead if you prefer.
  `((div ([class ,(dict 'bootstrap-row-class)])
         (div ([id "content"] ;Main content
               [class "span9"])
              ,@(dict 'bodies))
         (div ([class "span3"]) ;Tags/feeds/follow
              ;;,@(dict 'tocs)
              ,@(dict 'tags/feeds)
              ,@(dict 'follow)))))
