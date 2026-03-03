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
    (test-case "expect with no arguments gives bad syntax error"
      (check-exn #rx"expect: bad syntax"
                 (lambda ()
                   (parameterize ([current-namespace (make-base-namespace)])
                     (namespace-require 'recspecs)
                     (expand #'(expect))))))
    (test-case "expect with expression expectation works at expansion"
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display "hi") (number->string 123)))))))
    (test-case "expect with non-boolean #:strict? expands but fails at runtime"
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display "hi") "hi" #:strict? "yes"))))))
    (test-case "expect with invalid #:port expands but fails at runtime"
      (check-not-exn (lambda ()
                       (parameterize ([current-namespace (make-base-namespace)])
                         (namespace-require 'recspecs)
                         (expand #'(expect (display "hi") "hi" #:port invalid))))))
    (test-case "expect with valid #:port symbols works"
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
      (call-with-output-file
       tmp
       #:exists 'truncate/replace
       (lambda (out)
         (display "#lang at-exp racket\n(require recspecs)\n@expect[(displayln \"foo\")]{}\n" out)))
      (putenv "RECSPECS_UPDATE" "1")
      (dynamic-require tmp #f)
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (define expected
        (string-append "#lang at-exp racket\n"
                       "(require recspecs)\n"
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
      (flush-pending-updates!)
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
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       "@expect[(print 400)]{\n 400\n}\n"))
      (check-equal? (file->string tmp) expected))))

;; Regression test for issue #57: File corruption when RECSPECS_UPDATE=1
;; updates multiple @expect forms in the same file. Previously, byte positions
;; would become stale after the first update, causing subsequent updates to
;; corrupt the file. The fix tracks cumulative offsets per file.
(define multi-update-tests
  (test-suite "multi-update-tests"
    (test-case "updates multiple expect forms without corruption"
      (define tmp (make-temporary-file "multi~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang at-exp racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               ;; Three @expect forms that all need updating
                               (display "@expect[(displayln \"first\")]{}\n" out)
                               (display "@expect[(displayln \"second\")]{}\n" out)
                               (display "@expect[(displayln \"third\")]{}\n" out)))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       "@expect[(displayln \"first\")]{first\n}\n"
                       "@expect[(displayln \"second\")]{second\n}\n"
                       "@expect[(displayln \"third\")]{third\n}\n"))
      (check-equal? (file->string tmp) expected))

    ;; Test with varying output sizes to stress offset tracking
    (test-case "handles varying output sizes correctly"
      (define tmp (make-temporary-file "vary~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang at-exp racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               ;; Short, very long, then short output
                               (display "@expect[(display \"a\")]{}\n" out)
                               (display "@expect[(display (make-string 100 #\\x))]{}\n" out)
                               (display "@expect[(display \"b\")]{}\n" out)))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       "@expect[(display \"a\")]{a}\n"
                       "@expect[(display (make-string 100 #\\x))]{" (make-string 100 #\x) "}\n"
                       "@expect[(display \"b\")]{b}\n"))
      (check-equal? (file->string tmp) expected))

    ;; Note: Unicode content with multi-byte characters has known issues
    ;; with at-exp syntax position tracking. This test verifies ASCII-only
    ;; multi-update works. Unicode support may need separate investigation.

    ;; Test with many @expect forms to stress test offset accumulation
    (test-case "handles many expect forms"
      (define tmp (make-temporary-file "many~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang at-exp racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               ;; 10 @expect forms
                               (for ([i (in-range 10)])
                                 (fprintf out "@expect[(display ~a)]{}\n" i))))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       (apply string-append
                              (for/list ([i (in-range 10)])
                                (format "@expect[(display ~a)]{~a}\n" i i)))))
      (check-equal? (file->string tmp) expected))

    ;; Test updating stale expectations (non-empty braces)
    (test-case "updates stale expectations correctly"
      (define tmp (make-temporary-file "stale~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang at-exp racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               ;; Expectations with wrong/outdated content
                               (display "@expect[(display \"new1\")]{old1}\n" out)
                               (display "@expect[(display \"new2\")]{old2}\n" out)
                               (display "@expect[(display \"new3\")]{old3}\n" out)))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang at-exp racket/base\n"
                       "(require recspecs)\n\n"
                       "@expect[(display \"new1\")]{new1}\n"
                       "@expect[(display \"new2\")]{new2}\n"
                       "@expect[(display \"new3\")]{new3}\n"))
      (check-equal? (file->string tmp) expected))

    ;; Test with quoted string syntax (regular racket, not at-exp)
    (test-case "updates quoted string expectations correctly"
      (define tmp (make-temporary-file "quoted~a.rkt"))
      (call-with-output-file tmp
                             #:exists 'truncate/replace
                             (lambda (out)
                               (display "#lang racket/base\n" out)
                               (display "(require recspecs)\n\n" out)
                               ;; Regular quoted string expectations
                               (display "(expect (display \"hello\") \"wrong1\")\n" out)
                               (display "(expect (display \"world\") \"wrong2\")\n" out)
                               (display "(expect (display \"test\") \"wrong3\")\n" out)))
      (define tmp-str (path->string tmp))
      (putenv "RECSPECS_UPDATE" "1")
      (putenv "RECSPECS_UPDATE_TEST" tmp-str)
      (dynamic-require tmp #f)
      (flush-pending-updates!)
      (putenv "RECSPECS_UPDATE" "")
      (putenv "RECSPECS_UPDATE_TEST" "")
      (define expected
        (string-append "#lang racket/base\n"
                       "(require recspecs)\n\n"
                       "(expect (display \"hello\") \"hello\")\n"
                       "(expect (display \"world\") \"world\")\n"
                       "(expect (display \"test\") \"test\")\n"))
      (check-equal? (file->string tmp) expected))))

(define match-mode-tests
  (test-suite "match-mode-tests"
    (test-case "contains mode matches substring"
      (expect (display "hello world") "hello" #:match 'contains))
    (test-case "contains mode matches middle"
      (expect (display "foo bar baz") "bar" #:match 'contains))
    (test-case "regexp mode matches pattern"
      (expect (display "value: 42") "value: [0-9]+" #:match 'regexp))
    (test-case "regexp mode matches anywhere"
      (expect (display "the answer is 42!") "[0-9]+" #:match 'regexp))
    (test-case "equal mode is default"
      (expect (display "exact") "exact"))))

(module+ test
  (run-tests (test-suite "all"
               expect-tests
               expansion-tests
               syntax-error-tests
               at-exp-empty-tests
               at-exp-base-tests
               at-exp-newline-tests
               multi-update-tests
               match-mode-tests)))
