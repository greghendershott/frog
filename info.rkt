#lang setup/infotab
(define version "0.20")
(define collection 'multi)
(define deps '("base"
               "html-lib"
               "racket-index"
               "scribble-lib"
               "srfi-lite-lib"
               "web-server-lib"
               ("markdown" "0.15")
               ("rackjure" "0.7")
               "find-parent-dir"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
