#lang racket
(require rackunit
         rackunit/text-ui
         recspecs/shell)

(define shell-tests
  (test-suite "shell-tests"
    (test-case "cat session"
      (expect/shell "cat" "> hi\nhi\n> there\nthere\n"))
    (test-case "strict output"
      (expect/shell "cat" "> ok\nok\n" #:strict? #t))
    (test-case "bc calculator basic"
      (expect/shell "bc" "> 2+3\n5\n> 10*4\n40\n> quit\n"))
    (test-case "bc calculator with division"
      (expect/shell "bc" "> 15/3\n5\n> 22/7\n3\n> quit\n"))
    (test-case "bc calculator complex session"
      (expect/shell "bc" "> 2^8\n256\n> (5+3)*2\n16\n> scale=2\n> 22/7\n3.14\n> quit\n"))))

(module+ test
  (run-tests shell-tests))
