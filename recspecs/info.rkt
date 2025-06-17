#lang info

(define collection "recspecs")

(define scribblings (list (list "main.scrbl" (list 'multi-page) (list 'library) "recspecs")))

(define deps (list "recspecs-lib" "rackunit-lib" "scribble-lib" "base" "at-exp-lib"))

(define build-deps (list "racket-doc" "scribble-doc"))

(define license '(Apache-2.0))
