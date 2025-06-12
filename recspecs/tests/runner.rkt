#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define (limit-memory thunk)
  (call-in-nested-thread (lambda ()
                           (custodian-limit-memory (current-custodian) (* 1024 1024))
                           (thunk))))

(define (capture-errors thunk)
  (parameterize ([current-error-port (current-output-port)])
    (thunk)))

(define runner-tests
  (test-suite "runner-tests"
    (test-case "memory limit"
      (parameterize ([recspecs-runner limit-memory])
        (expect-exn (make-bytes (* 2 1024 1024)) "out of memory")))
    (test-case "redirect errors"
      (parameterize ([recspecs-runner capture-errors])
        (expect (display "oops" (current-error-port)) "oops")))))

(module+ test
  (run-tests runner-tests))
