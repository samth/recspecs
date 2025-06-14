#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define whitespace-tests
  (test-suite "whitespace-tests"
    (test-case "flexible"
      (expect (begin
                (display " Be more")
                (newline))
              " Be more ")
      (expect (begin
                (newline)
                (display "flexible")
                (newline))
              " flexible "))
    (test-case "newline combos strict"
      (expect (display " hello") " hello" #:strict? #t)
      (expect (display " hello\n") " hello\n" #:strict? #t)
      (expect (display " hello\n\n") " hello\n\n" #:strict? #t)
      (expect (display "\n hello") "\n hello" #:strict? #t)
      (expect (display "\n hello\n") "\n hello\n" #:strict? #t)
      (expect (display "\n hello\n\n") "\n hello\n\n" #:strict? #t)
      (expect (display "\n\n hello") "\n\n hello" #:strict? #t)
      (expect (display "\n\n hello\n") "\n\n hello\n" #:strict? #t)
      (expect (display "\n\n hello\n\n") "\n\n hello\n\n" #:strict? #t))))

(module+ test
  (run-tests whitespace-tests))
