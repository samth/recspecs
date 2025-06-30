#lang at-exp racket
(require rackunit
         rackunit/text-ui
         recspecs/shell)

(define shell-tests
  (test-suite "shell-tests"
    (test-case "cat session"
      @expect/shell["cat"]{> hi
hi
> there
there
})
    (test-case "strict output"
      @expect/shell["cat" #:strict? #t]{> ok
ok
})
    (test-case "bc calculator basic"
      @expect/shell["bc"]{> 2+3
5
> 10*4
40
> quit
})
    (test-case "bc calculator with division"
      @expect/shell["bc"]{> 15/3
5
> 22/7
3
> quit
})
    (test-case "bc calculator complex session"
      @expect/shell["bc"]{> 2^8
256
> (5+3)*2
16
> scale=2
> 22/7
3.14
> quit
})))

(module+ test
  (run-tests shell-tests))
