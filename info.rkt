#lang setup/infotab
(define version "0.26")
(define collection 'multi)
(define deps '("base"
               "find-parent-dir"
               "html-lib"
               ["markdown" "0.24"]
               "racket-index"
               "scribble-lib"
               "srfi-lite-lib"
               ["racket" "6.0.1"]
               ["rackjure" "0.9"]
               "web-server-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))
(define test-omit-paths '("example/"))
