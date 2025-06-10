#lang racket
(require racket/string)
(require rackunit rackunit/text-ui racket/file)
(require recspecs)

;; local copies of private update functions
(define (update-file path pos span new-str)
  (define bs (file->bytes path))
  (define start (sub1 pos))
  (define before (subbytes bs 0 start))
  (define after (subbytes bs (+ start span)))
  (define new-bs (bytes-append before (string->bytes/utf-8 (format "~s" new-str)) after))
  (call-with-output-file path #:exists 'truncate/replace
    (lambda (out) (write-bytes new-bs out))))

(define (update-file-entire path _pos _span new-str)
  (call-with-output-file path #:exists 'truncate/replace
    (lambda (out) (display new-str out))))

(define (find-pos str sub)
  (define m (regexp-match-positions (regexp-quote sub) str))
  (and m (add1 (caar m))))

(define update-tests
  (test-suite
   "update-tests"
   (test-case "update-file replaces substring"
     (define tmp (make-temporary-file "update~a.rkt"))
     (define before "(expect (displayln \"hello\") \"\")")
     (call-with-output-file tmp #:exists 'truncate/replace
       (lambda (out) (display before out)))
     (define pos (find-pos before "\"\""))
     (update-file tmp pos 2 "hello\n")
     (define after (file->string tmp))
     (delete-file tmp)
     (check-equal? after "(expect (displayln \"hello\") \"hello\\n\")"))

   (test-case "update-file-entire replaces file"
     (define tmp (make-temporary-file "expect-out~a.txt"))
     (call-with-output-file tmp #:exists 'truncate/replace
       (lambda (out) (display "old" out)))
     (update-file-entire tmp 0 0 "new")
     (define result (file->string tmp))
     (delete-file tmp)
     (check-equal? result "new"))))

(module+ test
  (run-tests update-tests))
