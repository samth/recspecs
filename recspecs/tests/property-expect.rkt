#lang racket
(require rackunit
         rackunit/text-ui
         racket/file
         rackcheck
         recspecs)

;; Expect with strings
(define prop-expect-string (property ([s (gen:string)]) (expect (display s) s) #t))

;; Expect with numbers
(define prop-expect-number
  (property ([n (gen:integer-in -1000 1000)]) (expect (display n) (number->string n)) #t))

;; Expect-exn with strings
(define prop-expect-exn-string
  (property ([s (gen:string)]) (expect-exn (raise (exn:fail s (current-continuation-marks))) s) #t))

;; Expect-exn with numbers
(define prop-expect-exn-number
  (property ([n (gen:integer-in -1000 1000)])
            (expect-exn (raise (exn:fail (number->string n) (current-continuation-marks)))
                        (number->string n))
            #t))

;; Expect-file with strings
(define prop-expect-file-string
  (property ([s (gen:string)])
            (define tmp (make-temporary-file "exp~a.txt"))
            (call-with-output-file tmp #:exists 'truncate/replace (lambda (out) (display s out)))
            (expect-file (display s) (path->string tmp))
            (delete-file tmp)
            #t))

;; Expect-file with numbers
(define prop-expect-file-number
  (property ([n (gen:integer-in -1000 1000)])
            (define tmp (make-temporary-file "exp~a.txt"))
            (call-with-output-file tmp #:exists 'truncate/replace (lambda (out) (display n out)))
            (expect-file (display n) (path->string tmp))
            (delete-file tmp)
            #t))

(define property-tests
  (test-suite "property-expect"
    (check-property prop-expect-string)
    (check-property prop-expect-number)
    (check-property prop-expect-exn-string)
    (check-property prop-expect-exn-number)
    (check-property prop-expect-file-string)
    (check-property prop-expect-file-number)))

(module+ test
  (run-tests property-tests))
