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
    (expect/print (+ 1 1) "2")
    (expect/pretty (list 1 2) "'(1 2)\n")
    (parameterize ([pretty-print-columns 3])
      (expect/pretty (list 1 2 3 4) "'(1\n  2\n  3\n  4)\n"))
    (expect-exn (raise (exn:fail "oops" (current-continuation-marks))) "oops")))

;; Macro expansion with no expectation should not error
(define expansion-tests
  (test-suite "expansion-tests"
    (test-case "expect without string doesn't error"
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display 5)))))))))

;; Test improved syntax error messages
(define syntax-error-tests
  (test-suite "syntax-error-tests"
    (test-case "expect with no arguments"
      (check-exn #rx"expect: missing expression argument"
                 (lambda ()
                   (parameterize ([current-namespace (make-base-namespace)])
                     (namespace-require 'recspecs)
                     (expand #'(expect))))))
    (test-case "expect with non-string expectation"
      (check-exn #rx"expected expectation string"
                 (lambda ()
                   (parameterize ([current-namespace (make-base-namespace)])
                     (namespace-require 'recspecs)
                     (expand #'(expect (display "hi") 123))))))
    (test-case "expect with non-boolean #:strict?"
      (check-exn #rx"expected boolean value"
                 (lambda ()
                   (parameterize ([current-namespace (make-base-namespace)])
                     (namespace-require 'recspecs)
                     (expand #'(expect (display "hi") "hi" #:strict? "yes"))))))
    (test-case "expect with invalid #:port"
      (check-exn #rx"expected port symbol"
                 (lambda ()
                   (parameterize ([current-namespace (make-base-namespace)])
                     (namespace-require 'recspecs)
                     (expand #'(expect (display "hi") "hi" #:port invalid))))))
    (test-case "expect with valid #:port symbols"
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display "hi") "hi" #:port 'stdout)))))
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display "hi") "hi" #:port 'stderr)))))
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display "hi") "hi" #:port 'both))))))))

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

(define at-exp-newline-tests
  (test-suite "at-exp-newline-tests"
    (test-case "updates at-exp with newline"
      (define tmp (make-temporary-file "newline~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang at-exp racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               (display "@expect[(print 400)]{\n 4000\n}\n" out)))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       "@expect[(print 400)]{\n 400\n}\n"))
      (check-equal? (file->string tmp) expected))))

(module+ test
  (run-tests (test-suite "all"
               expect-tests
               expansion-tests
               syntax-error-tests
               at-exp-empty-tests
               at-exp-base-tests
               at-exp-newline-tests)))
