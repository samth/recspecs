#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define stderr-tests
  (test-suite "stderr-tests"
    (test-case "expect stderr"
      (expect (display "oops" (current-error-port)) "oops" #:stderr? #t))
    (test-case "capture stderr"
      (check-equal? (capture-output (lambda () (display "err" (current-error-port))) #:stderr? #t)
                    "err"))
    (test-case "both ports"
      (expect (begin
                (display "warn" (current-error-port))
                (display "out"))
              "warnout"
              #:stderr? 'both))))

(module+ test
  (run-tests stderr-tests))
