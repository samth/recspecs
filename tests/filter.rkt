#lang racket
(require rackunit
         rackunit/text-ui
         recspecs)

(define filter-tests
  (test-suite "filter-tests"
    (test-case "output-filter"
      (parameterize ([recspecs-output-filter
                      (lambda (s) (regexp-replace #px"timestamp: \\d+" s "timestamp: <ts>"))])
        (expect (displayln "timestamp: 123") "timestamp: <ts>\n")))))

(module+ test
  (run-tests filter-tests))
