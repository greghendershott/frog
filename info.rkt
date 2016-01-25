#lang setup/infotab
(define version "0.24")
(define collection 'multi)
(define deps '("base"
               "find-parent-dir"
               "html-lib"
               ["markdown" "0.22"]
               "racket-index"
               "scribble-lib"
               "srfi-lite-lib"
               ["racket" "6.0"]
               ["rackjure" "0.9"]
               "web-server-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
