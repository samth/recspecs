#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define expect-tests
  (test-suite
   "expect-tests"
   (expect (display "hello") "hello")
  (expect (begin
            (displayln "hello")
            (displayln (+ 1 2)))
          "hello\n" "3\n")
  (expect (void) "")
  ;; Empty expectation string via implicit "" argument
  (expect (void))
  ;; Flexible whitespace matching
  (expect (display "hello") "  hello  \n")
  ;; Strict matching
  (expect (display "strict") "strict" #:strict? #t)
  (expect-exn (raise (exn:fail "oops" (current-continuation-marks))) "oops")))

;; Macro expansion with no expectation should not error
(define expansion-tests
  (test-suite
   "expansion-tests"
   (test-case "expect without string doesn't error"
     (check-not-exn
      (lambda ()
        (parameterize ([current-namespace (make-base-namespace)])
          (namespace-require 'recspecs)
          (expand #'(expect (display 5)))))))))

(module+ test
  (run-tests (test-suite "all" expect-tests expansion-tests)))
