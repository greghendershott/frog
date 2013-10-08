#lang setup/infotab
(define version "0.7")
(define collection 'multi)
;; Please duplicate all _direct_ deps in .travis.yml, too.
(define deps '(("markdown" "0.5")
               "rackjure"
               "find-parent-dir"))
