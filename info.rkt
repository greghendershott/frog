#lang info
(define version "0.29")
(define collection 'multi)
(define deps '(["base" #:version "6.1"]
               "find-parent-dir"
               "html-lib"
               ["markdown" #:version "0.25"]
               "racket-index"
               ["rackjure" #:version "0.9"]
               "reprovide-lang"
               "scribble-lib"
               "scribble-text-lib"
               "srfi-lite-lib"
               "web-server-lib"))
(define build-deps '("at-exp-lib"
                     "net-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-doc"
                     "scribble-text-lib"
                     "web-server-doc"))
(define test-omit-paths '("example/"))
