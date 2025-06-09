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
          "hello\n" "3\n")
  (expect (void) "")
  ;; Flexible whitespace matching
  (expect (display "hello") "  hello  \n")
  ;; Strict matching
  (expect (display "strict") "strict" #:strict? #t)
  (expect-exn (raise (exn:fail "oops" (current-continuation-marks))) "oops")))

(module+ test
  (run-tests expect-tests))
