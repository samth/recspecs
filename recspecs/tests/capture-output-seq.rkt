#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define capture-seq-tests
  (test-suite "capture-output-seq"
    (test-case "capture sequential output"
      (define first (capture-output (lambda () (displayln "hello"))))
      (expect (display first) "hello\n")
      (define second (capture-output (lambda () (display "no newline"))))
      (expect (displayln (string-upcase second)) "NO NEWLINE\n"))))

(module+ test
  (run-tests capture-seq-tests))
