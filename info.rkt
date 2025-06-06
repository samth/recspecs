#lang info

(define collection "recspecs")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "recspecs")))

(define deps
  (list "rackunit-lib"
        "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
