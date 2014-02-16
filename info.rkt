#lang setup/infotab
(define version "0.14")
(define collection 'multi)
(define deps '("base"
               "html-lib"
               "racket-index"
               "scribble-lib"
               "srfi-lite-lib"
               "web-server-lib"
               ("markdown" "0.12")
               "rackjure"
               "find-parent-dir"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
