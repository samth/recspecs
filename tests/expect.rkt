#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define expect-tests
  (test-suite
   "expect-tests"
   (expect (display "hello") "hello")
   (expect (begin
             (displayln "hello")
             (displayln (+ 1 2)))
           "hello\n3\n")
   (expect (void) "")))

(module+ test
  (run-tests expect-tests))
