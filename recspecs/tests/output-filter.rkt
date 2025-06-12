#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define (strip-digits s)
  (regexp-replace* #px"[0-9]+" s ""))

(define filter-tests
  (test-suite "filter-tests"
    (expect (display "a1b2") "ab")
    (expect-exn (raise (exn:fail "err1" (current-continuation-marks))) "err")))

(module+ test
  (parameterize ([recspecs-output-filter strip-digits])
    (run-tests filter-tests)))
