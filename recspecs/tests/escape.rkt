#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define escape-tests
  (test-suite "escape-tests"
    (test-case "carriage return"
      (expect (display "a\r\nb") " \n a\n b\n ")
      (expect (display "a\r\nb") "a\r\nb" #:strict? #t))
    (test-case "tab"
      (expect (display "a\tb") "a\tb")
      (expect (display "a\tb") "a\tb" #:strict? #t))
    (test-case "quote"
      (expect (display "a\"b") "a\"b")
      (expect (display "a\"b") "a\"b" #:strict? #t))
    (test-case "trailing carriage"
      (expect (display "a\r\nb\r\n") " \n a\n b\n ")
      (expect (display "a\r\nb\r\n") "a\r\nb\r\n" #:strict? #t))))

(module+ test
  (run-tests escape-tests))
