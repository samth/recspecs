#lang racket/base

(require racket/file
         racket/port
         rackunit
         (for-syntax racket/base
                     syntax/parse))

(provide expect)

;; Returns #t when expectations should be updated instead of reported as
;; failures. The update mode is enabled when the environment variable
;; RECSPECS_UPDATE is set to any value.
(define (update-mode?)
  (and (getenv "RECSPECS_UPDATE") #t))

(define (update-file path pos span new-str)
  ;; Replace the expectation string located at [pos, pos+span) in the file
  ;; at `path` with the printed representation of `new-str`.
  (define bs (file->bytes path))
  (define before (subbytes bs 0 pos))
  (define after (subbytes bs (+ pos span)))
  (define new-bs (bytes-append before
                               (string->bytes/utf-8 (format "~s" new-str))
                               after))
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (write-bytes new-bs out))))

(define (run-expect thunk expected path pos span)
  ;; Returns a rackunit test that evaluates `thunk`, captures anything printed
  ;; to the current output port and compares it to `expected`. When update mode
  ;; is enabled and the values differ, the source file is rewritten instead of
  ;; failing the test.
  (define name (if path
                   (format "~a:~a" path pos)
                   "expect"))
  (test-case name
    (define actual (with-output-to-string thunk))
    (if (and path (update-mode?) (not (string=? actual expected)))
        (begin (update-file path pos span actual)
               (printf "Updated expectation in ~a\n" path))
        (check-equal? actual expected))))

(define-syntax (expect stx)
  (syntax-parse stx
    [(_ expr expected:str)
     (define src (syntax-source #'expected))
     (define pos (or (syntax-position #'expected) 0))
     (define span (or (syntax-span #'expected)
                      (string-length (syntax-e #'expected))))
     #`(run-expect (lambda () expr)
                   expected
                   #,(and src (path->string src))
                   #,pos
                   #,span)]))
