#lang at-exp racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define expect-tests
  (test-suite
   "expect-tests"
   @expect[(display "hello")] {hello}
   @expect[(begin
             (displayln "hello")
             (displayln (+ 1 2)))] {hello
3
}
   (expect (displayln "foo") "foo" "\n")
   (expect (void) "")))

(module+ test
  (run-tests expect-tests))
