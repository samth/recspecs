#lang at-exp racket
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
      @expect/shell["bc" @~a{> 2^8
256
> (5+3)*2
16
> scale=2
> 22/7
3.14
> quit
}])))

(module+ test
  (run-tests shell-tests))
