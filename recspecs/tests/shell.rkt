#lang racket
(require rackunit
         rackunit/text-ui
         recspecs/shell)

(define shell-tests
  (test-suite "shell-tests"
    (test-case "cat session"
      (expect/shell "cat" "> hi\nhi\n> there\nthere\n"))
    (test-case "strict output"
      (expect/shell "cat" "> ok\nok\n" #:strict? #t))))

(module+ test
  (run-tests shell-tests))
