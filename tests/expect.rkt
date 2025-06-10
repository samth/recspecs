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

;; Ensure that @expect with empty braces is updated
(define at-exp-empty-tests
  (test-suite
   "at-exp-empty-tests"
   (test-case "updates empty at-exp braces"
     (define tmp (make-temporary-file "tmp~a.rkt"))
     (define main-path (build-path (current-directory) ".." "main.rkt"))
     (call-with-output-file tmp
       #:exists 'truncate/replace
       (lambda (out)
         (fprintf out "#lang at-exp racket\n(require (file \"~a\"))\n@expect[(displayln \"foo\")]{}\n" (path->string main-path))))
     (putenv "RECSPECS_UPDATE" "1")
     (dynamic-require tmp #f)
     (putenv "RECSPECS_UPDATE" "")
      (define expected
        (string-append "#lang at-exp racket\n"
                       "(require (file \"" (path->string main-path) "\"))\n"
                       "@expect[(displayln \"foo\")]{" "foo\n" "}\n"))
     (check-equal? (file->string tmp) expected))))

(module+ test
  (run-tests (test-suite "all" expect-tests expansion-tests at-exp-empty-tests)))
