#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define ocaml-expectation-tests
  (test-suite "ocaml-expectation-tests"
    ;; expectation passes
    (expect (display "foo") "foo")
    ;; multiline and indentation matches
    (expect (begin
              (displayln "line 1")
              (displayln " line 2")
              (displayln "  line 3"))
            "line 1\n line 2\n  line 3\n")))

(define ocaml-output-tests
  (test-suite "ocaml-output-tests"
    (expect (let ([output (capture-output (lambda () (displayln "hello")))])
              (display "'")
              (display (string-upcase output))
              (display "'\n"))
            "'HELLO\n'")
    (expect (let ([output (capture-output (lambda () (display "string without line break")))])
              (display (string-upcase output)))
            "STRING WITHOUT LINE BREAK")
    (let ()
      (define output1 (capture-output (lambda () (display "first"))))
      (define output2 (capture-output (lambda () (display "second"))))
      (expect (display (string-upcase output2)) "SECOND")
      (expect (display (string-upcase output1)) "FIRST"))))

(define ocaml-unreachable-tests
  (test-suite "ocaml-unreachable-tests"
    (test-case "fails when reached"
      (check-exn exn:fail?
                 (lambda ()
                   (parameterize ([current-test-case-around (lambda (th) (th))])
                     (expect-unreachable (void))))))))

(define ocaml-uncaught-exn-tests
  (test-suite "ocaml-uncaught-exn-tests"
    (expect-exn (raise (exn:fail "dummy error" (current-continuation-marks))) "dummy error")))

;; Additional tests adapted from the OCaml expect library
(define ocaml-strict-tests
  (test-suite "ocaml-strict-tests"
    (expect (display "bar") "bar" #:strict? #t)
    (expect (display "with space ") "with space " #:strict? #t)))

(define ocaml-stderr-tests
  (test-suite "ocaml-stderr-tests"
    (expect (display "err" (current-error-port)) "err" #:port 'stderr)
    (expect (begin
              (display "out")
              (display "err" (current-error-port)))
            "outerr"
            #:port 'both)))

(define ocaml-multi-string-tests
  (test-suite "ocaml-multi-string-tests"
    (expect (display "abc") "a" "b" "c")))

(define ocaml-file-tests
  (test-suite "ocaml-file-tests"
    (expect-file (display "hello") "data/hello.txt")))

(define ocaml-with-expectation-tests
  (test-suite "ocaml-with-expectation-tests"
    (test-case "records and commits"
      (define e (make-expectation))
      (with-expectation e (display "hi"))
      (commit-expectation! e)
      (expect (display (expectation-out e)) "hi"))))

(module+ test
  (run-tests (test-suite "all"
               ocaml-expectation-tests
               ocaml-output-tests
               ocaml-unreachable-tests
               ocaml-uncaught-exn-tests
               ocaml-strict-tests
               ocaml-stderr-tests
               ocaml-multi-string-tests
               ocaml-file-tests
               ocaml-with-expectation-tests)))
