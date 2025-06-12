#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define expectation-tests
  (test-suite "expectation"
    (test-case "commit recorded output"
      (define e (make-expectation))
      (with-expectation e (display "a"))
      (commit-expectation! e)
      (check-true (expectation-committed? e))
      (check-equal? (expectation-out e) "a"))
    (test-case "skip recorded output"
      (define e (make-expectation))
      (with-expectation e (display "b"))
      (skip-expectation! e)
      (check-true (expectation-skip? e))
      (check-equal? (expectation-out e) "b"))
    (test-case "reset clears state"
      (define e (make-expectation))
      (with-expectation e (display "x"))
      (commit-expectation! e)
      (reset-expectation! e)
      (check-equal? (expectation-out e) "")
      (check-false (expectation-committed? e))
      (check-false (expectation-skip? e)))
    (test-case "works with expect"
      (define e (make-expectation))
      (with-expectation e (display "hi"))
      (expect (display (expectation-out e)) "hi"))
    (test-case "works with expect-file"
      (define e (make-expectation))
      (with-expectation e (display "hello\n"))
      (expect-file (display (expectation-out e)) "data/hello.txt"))
    (test-case "works with expect-exn"
      (define e (make-expectation))
      (with-expectation e (display "boom:"))
      (expect-exn (raise (exn:fail (string-append (expectation-out e) "err")
                                   (current-continuation-marks)))
                  "boom:err")
      (commit-expectation! e))
    (test-case "works with expect-unreachable"
      (define e (make-expectation))
      (with-expectation e (display "u"))
      (when #f
        (expect-unreachable (display (expectation-out e)))))))

(module+ test
  (run-tests expectation-tests))
