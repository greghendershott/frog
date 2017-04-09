#lang setup/infotab
(define version "0.26")
(define collection 'multi)
(define deps '("base"
               "find-parent-dir"
               "html-lib"
               ["markdown" "0.24"]
               "racket-index"
               ["racket" "6.0.1"]
               ["rackjure" "0.9"]
               "scribble-lib"
               "srfi-lite-lib"
               "web-server-lib"))
(define build-deps '("at-exp-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-doc"
                     "scribble-text-lib"
                     "web-server-doc"))
(define test-omit-paths '("example/"))
