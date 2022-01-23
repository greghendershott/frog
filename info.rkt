#lang info
(define version "0.30")
(define collection 'multi)
(define deps '(["base" #:version "6.3"]
               "find-parent-dir"
               "html-lib"
               ["markdown" #:version "0.25"]
               "racket-index"
               "reprovide-lang-lib"
               "scribble-lib"
               "scribble-text-lib"
               "srfi-lite-lib"
               "threading-lib"
               "web-server-lib"))
(define build-deps '("at-exp-lib"
                     "net-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-doc"
                     "scribble-text-lib"
                     "threading-doc"
                     "web-server-doc"))
(define test-omit-paths '("example/"))
