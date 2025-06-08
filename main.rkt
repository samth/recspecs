#lang racket/base

(require racket/file
         racket/port
         racket/list
         racket/string
         racket/vector
         racket/match
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

;; Split a string into lines without dropping trailing empty lines
(define (string->lines s)
  (define lines (regexp-split #px"\n" s))
  (if (regexp-match? #px"\n$" s)
      (append lines '(""))
      lines))

;; Compute a simple diff between two sequences of lines using the
;; longest common subsequence algorithm.
(define (lines-diff a-lines b-lines)
  (define m (length a-lines))
  (define n (length b-lines))
  (define tbl
    (for/vector ([i (in-range (add1 m))])
      (make-vector (add1 n) 0)))
  ;; fill table
  (for ([i (in-range m)])
    (for ([j (in-range n)])
      (vector-set! (vector-ref tbl (add1 i)) (add1 j)
                   (if (string=? (list-ref a-lines i) (list-ref b-lines j))
                       (add1 (vector-ref (vector-ref tbl i) j))
                       (max (vector-ref (vector-ref tbl i) (add1 j))
                            (vector-ref (vector-ref tbl (add1 i)) j))))))
  ;; backtrack
  (define diffs '())
  (let loop ([i m] [j n])
    (cond
      [(and (> i 0) (> j 0)
             (string=? (list-ref a-lines (sub1 i)) (list-ref b-lines (sub1 j))))
       (set! diffs (cons (cons 'same (list-ref a-lines (sub1 i))) diffs))
       (loop (sub1 i) (sub1 j))]
      [(and (> j 0)
            (or (= i 0)
                (>= (vector-ref (vector-ref tbl i) (sub1 j))
                    (vector-ref (vector-ref tbl (sub1 i)) j))))
       (set! diffs (cons (cons 'add (list-ref b-lines (sub1 j))) diffs))
       (loop i (sub1 j))]
      [(> i 0)
       (set! diffs (cons (cons 'del (list-ref a-lines (sub1 i))) diffs))
       (loop (sub1 i) j)]))
  diffs)

;; Render a diff as a string with ANSI color codes
(define (pretty-diff expected actual)
  (define diffs (lines-diff (string->lines expected)
                            (string->lines actual)))
  (define (color c s)
    (string-append "\x1b[" c "m" s "\x1b[0m"))
  (string-join
   (for/list ([d diffs])
     (match d
       [(cons 'same l) (string-append "  " l)]
       [(cons 'add l) (color "32" (string-append "+ " l))]
       [(cons 'del l) (color "31" (string-append "- " l))]))
   "\n"))

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
    (cond
      [(and path (update-mode?) (not (string=? actual expected)))
       (update-file path pos span actual)
       (printf "Updated expectation in ~a\n" path)]
      [(string=? actual expected)
       (check-equal? actual expected)]
      [else
       (displayln "Diff:" (current-error-port))
       (displayln (pretty-diff expected actual) (current-error-port))
       (check-equal? actual expected)])))

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
