#lang racket
(require rackunit
         rackunit/text-ui
         rackcheck
         recspecs)

(define prop-capture-output
  (property ([s (gen:string)]) (string=? (capture-output (lambda () (display s))) s)))

(define prop-capture-stderr
  (property ([s (gen:string)])
            (string=? (capture-output (lambda () (display s (current-error-port))) #:stderr? #t) s)))

(define prop-capture-both
  (property ([s1 (gen:string)] [s2 (gen:string)])
            (string=? (capture-output (lambda ()
                                        (display s1 (current-error-port))
                                        (display s2))
                                      #:stderr? 'both)
                      (string-append s1 s2))))

;; Non-string values
(define prop-capture-number
  (property ([n (gen:integer-in -1000 1000)])
            (string=? (capture-output (lambda () (display n))) (number->string n))))

(define prop-capture-number-stderr
  (property ([n (gen:integer-in -1000 1000)])
            (string=? (capture-output (lambda () (display n (current-error-port))) #:stderr? #t)
                      (number->string n))))

(define prop-capture-number-both
  (property ([n1 (gen:integer-in -1000 1000)] [n2 (gen:integer-in -1000 1000)])
            (string=? (capture-output (lambda ()
                                        (display n1 (current-error-port))
                                        (display n2))
                                      #:stderr? 'both)
                      (string-append (number->string n1) (number->string n2)))))

(define property-tests
  (test-suite "property-capture-output"
    (check-property prop-capture-output)
    (check-property prop-capture-stderr)
    (check-property prop-capture-both)
    (check-property prop-capture-number)
    (check-property prop-capture-number-stderr)
    (check-property prop-capture-number-both)))

(module+ test
  (run-tests property-tests))
