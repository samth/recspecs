#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define unreachable-tests
  (test-suite "unreachable-tests"
    (test-case "fails when reached"
      (check-exn exn:fail?
                 (lambda ()
                   (parameterize ([current-test-case-around (lambda (th) (th))])
                     (expect-unreachable (void))))))))

(define update-tests
  (test-suite "update-tests"
    (test-case "updates source when reached in update mode"
      (define tmp (make-temporary-file "unreach~a.rkt"))
      (call-with-output-file
       tmp
       #:exists 'truncate/replace
       (lambda (out)
         (fprintf out "#lang racket\n(require recspecs)\n(expect-unreachable (displayln \"x\"))\n")))
      (putenv "RECSPECS_UPDATE" "1")
      (dynamic-require tmp #f)
      (putenv "RECSPECS_UPDATE" "")
      (define expected "#lang racket\n(require recspecs)\n(displayln \"x\")\n")
      (check-equal? (file->string tmp) expected)
      (delete-file tmp))))

(module+ test
  (run-tests (test-suite "all"
               unreachable-tests
               update-tests)))
