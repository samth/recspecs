#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define capture-tests
  (test-suite "capture-output"
    (test-case "returns output"
      (check-equal? (capture-output (lambda () (display "hi"))) "hi"))
    (test-case "both streams"
      (check-equal? (capture-output (lambda ()
                                      (display "err" (current-error-port))
                                      (display "out"))
                                    #:port 'both)
                    "errout"))))

(define split-tests
  (test-suite "capture-output/split"
    (test-case "separates stdout and stderr"
      (define-values (out err)
        (capture-output/split (lambda ()
                                (display "stdout-text")
                                (display "stderr-text" (current-error-port)))))
      (check-equal? out "stdout-text")
      (check-equal? err "stderr-text"))
    (test-case "empty stderr"
      (define-values (out err)
        (capture-output/split (lambda () (display "only-out"))))
      (check-equal? out "only-out")
      (check-equal? err ""))
    (test-case "empty stdout"
      (define-values (out err)
        (capture-output/split (lambda () (display "only-err" (current-error-port)))))
      (check-equal? out "")
      (check-equal? err "only-err"))))

(module+ test
  (run-tests capture-tests)
  (run-tests split-tests))
