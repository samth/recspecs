#lang racket
(require rackunit
         rackunit/text-ui
         racket/file
         recspecs)

;; expectation files live under the "data" subdirectory

(define file-tests
  (test-suite "file-tests"
    (test-case "single line"
      (expect-file (displayln "hello") "data/hello.txt"))
    (test-case "multi line"
      (expect-file (begin
                     (displayln "a")
                     (displayln "b"))
                   "data/multi.txt"))
    (test-case "strict mode"
      (define strict-file (make-temporary-file "strict~a.txt"))
      (call-with-output-file strict-file
                             #:exists 'truncate/replace
                             (lambda (out) (display "strict" out)))
      (expect-file (display "strict") (path->string strict-file) #:strict? #t)
      (delete-file strict-file))))

(define update-tests
  (test-suite "update-tests"
    (test-case "updates expectation file"
      (define src "data/update.txt")
      (define tmp-out (make-temporary-file "out~a.txt"))
      (call-with-output-file tmp-out
                             #:exists 'truncate/replace
                             (lambda (out) (display (file->string src) out)))
      (define tmp-test (make-temporary-file "test~a.rkt"))
      (call-with-output-file
       tmp-test
       #:exists 'truncate/replace
       (lambda (out)
         (fprintf out
                  "#lang racket\n(require recspecs)\n(expect-file (displayln \"new\") \"~a\")\n"
                  (path->string tmp-out))))
      (putenv "RECSPECS_UPDATE" "1")
      (dynamic-require tmp-test #f)
      (putenv "RECSPECS_UPDATE" "")
      (check-equal? (file->string tmp-out) "new\n")
      (delete-file tmp-out)
      (delete-file tmp-test))))

(module+ test
  (run-tests (test-suite "all"
               file-tests
               update-tests)))
