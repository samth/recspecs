#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define expect-tests
  (test-suite "expect-tests"
    (expect (display "hello") "hello")
    (expect (begin
              (displayln "hello")
              (displayln (+ 1 2)))
            "hello\n"
            "3\n")
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
  (test-suite "expansion-tests"
    (test-case "expect without string doesn't error"
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display 5)))))))))

;; Ensure that @expect with empty braces is updated
(define at-exp-empty-tests
  (test-suite "at-exp-empty-tests"
    (test-case "updates empty at-exp braces"
      (define tmp (make-temporary-file "tmp~a.rkt"))
      (define main-path (build-path (current-directory) ".." ".." "recspecs-lib" "main.rkt"))
      (call-with-output-file
       tmp
       #:exists 'truncate/replace
       (lambda (out)
         (fprintf out
                  "#lang at-exp racket\n(require (file \"~a\"))\n@expect[(displayln \"foo\")]{}\n"
                  (path->string main-path))))
      (putenv "RECSPECS_UPDATE" "1")
      (dynamic-require tmp #f)
      (putenv "RECSPECS_UPDATE" "")
      (define expected
        (string-append "#lang at-exp racket\n"
                       "(require (file \""
                       (path->string main-path)
                       "\"))\n"
                       "@expect[(displayln \"foo\")]{"
                       "foo\n"
                       "}\n"))
      (check-equal? (file->string tmp) expected))))

;; Ensure that @expect works in `#lang at-exp racket/base` files
(define at-exp-base-tests
  (test-suite "at-exp-base-tests"
    (test-case "updates at-exp racket/base"
      (define tmp (make-temporary-file "base~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang at-exp racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               (display "(expect (print 3) \"3\")\n\n" out)
                               (display "@expect[(print 400)]{}\n" out)))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       "(expect (print 3) \"3\")\n\n"
                       "@expect[(print 400)]{400}\n"))
      (check-equal? (file->string tmp) expected))))

(module+ test
  (run-tests (test-suite "all"
               expect-tests
               expansion-tests
               at-exp-empty-tests
               at-exp-base-tests)))
