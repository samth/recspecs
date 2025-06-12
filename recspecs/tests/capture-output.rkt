#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define capture-tests
  (test-suite "capture-output"
    (test-case "returns output"
      (check-equal? (capture-output (lambda () (display "hi"))) "hi"))))

(module+ test
  (run-tests capture-tests))
