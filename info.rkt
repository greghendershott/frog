#lang setup/infotab
(define version "0.11")
(define collection 'multi)
(define deps '("base"
               "html-lib"
               "racket-index"
               "scribble-lib"
               "srfi-lite-lib"
               "web-server-lib"
               ("markdown" "0.9")
               "rackjure"
               "find-parent-dir"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
