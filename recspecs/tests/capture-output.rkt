#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define capture-tests
  (test-suite "capture-output"
    (test-case "returns output"
      (check-equal? (capture-output (lambda () (display "hi"))) "hi"))
    (test-case "both streams"
      (check-equal? (capture-output (lambda ()
                                      (display "err" (current-error-port))
                                      (display "out"))
                                    #:port 'both)
                    "errout"))))

(module+ test
  (run-tests capture-tests))
