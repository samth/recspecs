#lang at-exp racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define expect-at-exp-tests
  (test-suite
   "expect-at-exp-tests"
   @expect[(display "hi")]{hi}
   @expect[(displayln (+ 2 3))]{5
}
   ))

(module+ test
  (run-tests expect-at-exp-tests))
