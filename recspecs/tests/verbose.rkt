#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define recorded '())

(define (make-record-port)
  (make-output-port 'record
                    always-evt
                    (lambda (bs start end non-block? break?)
                      (set! recorded
                            (append recorded (list (bytes->string/utf-8 (subbytes bs start end)))))
                      (- end start))
                    (lambda () (void))))

(define verbose-tests
  (test-suite "verbose-tests"
    (test-case "echoes output"
      (set! recorded '())
      (define port (make-record-port))
      (parameterize ([recspecs-verbose? #t]
                     [current-output-port port])
        (run-tests (test-suite "sub"
                     (expect (begin
                               (display "a")
                               (display "b"))
                             "ab"))))
      (check-equal? (take recorded 2) '("a" "b")))))

(module+ test
  (run-tests verbose-tests))
