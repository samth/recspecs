#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define example-tests
  (test-suite "example-tests"
    (test-case "foo"
      (expect (begin
                (displayln "line1")
                (displayln '(1 2 3)))
              "line1\n"
              "(1 2 3)\n"))
    (test-case "hello world"
      (expect (display "hello, world!") "hello, world!")
      (expect (display "hello, world!") "hello, world!" #:strict? #t))
    (test-case "weird escaping"
      (expect (display "I need |}weird escaping") "I need |}weird escaping"))))

(module+ test
  (run-tests example-tests))
