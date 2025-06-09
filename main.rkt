#lang racket/base

(require racket/file
         racket/port
         racket/list
         racket/string
         racket/vector
         racket/match
         rackunit
         (for-syntax racket/base
                     syntax/parse
                     racket/list))

(provide expect
         expect-file
         expect-exn)

;; Normalize a string by trimming leading and trailing whitespace and removing
;; common indentation from all lines. This is used when comparing expectation
;; strings in non strict mode.
(define (normalize-string s)
  (define trimmed (string-trim s))
  (define lines (regexp-split #px"\r?\n" trimmed))
  (define non-empty-lines
    (filter (lambda (l) (regexp-match? #px"\\S" l)) lines))
  (define min-indent
    (if (null? non-empty-lines)
        0
        (apply min
               (map (lambda (l)
                      (string-length (car (regexp-match #px"^[ \t]*" l))))
                    non-empty-lines))))
  (define dedented
    (for/list ([l lines])
      (string-trim (substring l (min (string-length l) min-indent)))))
  (string-join dedented "\n"))

;; Returns #t when expectations should be updated instead of reported as
;; failures. The update mode is enabled when the environment variable
;; RECSPECS_UPDATE is set to any value. When the optional
;; RECSPECS_UPDATE_TEST is set, only test cases whose names contain the
;; given string are updated.
(define (update-mode? [name #f])
  (define update? (getenv "RECSPECS_UPDATE"))
  (and update?
       (let ([filter (getenv "RECSPECS_UPDATE_TEST")])
         (if filter
             (and name (string-contains? name filter))
             #t))))

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

;; Replace the entire file at `path` with `new-str`.
(define (update-file-entire path _pos _span new-str)
  (call-with-output-file path
    #:exists 'truncate/replace
    (lambda (out)
      (display new-str out))))

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

(define (run-expect thunk expected path pos span
                    [update update-file]
                    #:strict [strict? #f])
  ;; Returns a rackunit test that evaluates `thunk`, captures anything printed
  ;; to the current output port and compares it to `expected`. When update mode
  ;; is enabled and the values differ, the source file is rewritten instead of
  ;; failing the test.
  (define name (if path
                   (format "~a:~a" path pos)
                   "expect"))
  (test-case name
    (define actual (with-output-to-string thunk))
    (define equal?
      (if strict?
          (string=? actual expected)
          (string=? (normalize-string actual)
                    (normalize-string expected))))
    (cond
      [(and path (update-mode? name) (not equal?))
       (update path pos span actual)
       (printf "Updated expectation in ~a\n" path)]
      [equal?
       (check-true equal?)]
      [else
       (displayln "Diff:" (current-error-port))
       (displayln (pretty-diff expected actual) (current-error-port))
       (check-true equal?)])))

(define (run-expect-exn thunk expected path pos span
                        [update update-file]
                        #:strict [strict? #f])
  (define name (if path
                   (format "~a:~a" path pos)
                   "expect-exn"))
  (test-case name
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (define actual (exn-message e))
                       (define equal?
                         (if strict?
                             (string=? actual expected)
                             (string=? (normalize-string actual)
                                       (normalize-string expected))))
                       (cond
                         [(and path (update-mode? name) (not equal?))
                          (update path pos span actual)
                          (printf "Updated expectation in ~a\n" path)]
                         [equal?
                          (check-true equal?)]
                         [else
                          (displayln "Diff:" (current-error-port))
                          (displayln (pretty-diff expected actual) (current-error-port))
                          (check-true equal?)]))])
      (begin
        (thunk)
        (fail "expected an exception")))))

(define-syntax (expect stx)
  (syntax-parse stx
    [(_ expr expected-first:str expected-rest:str ...
        (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     (define expect-list (syntax->list #'(expected-first expected-rest ...)))
     (define first #'expected-first)
     (define last-syn (if (null? (syntax->list #'(expected-rest ...)))
                         #'expected-first
                         (last expect-list)))
     (define src (syntax-source first))
     (define pos (or (syntax-position first) 0))
     (define span (- (+ (or (syntax-position last-syn) 0)
                        (or (syntax-span last-syn)
                            (string-length (syntax-e last-syn))))
                     pos))
     #`(run-expect (lambda () expr)
                   (string-append #,@expect-list)
                   #,(and src (path->string src))
                   #,pos
                   #,span
                   #:strict s?)]
    [(_ expr (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     #'(run-expect (lambda () expr) "" #f 0 0 #:strict s?)]))

(define-syntax (expect-file stx)
  (syntax-parse stx
    [(_ expr path:str (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     #'(let ([p path])
         (run-expect (lambda () expr)
                     (call-with-input-file p port->string)
                     (path->string p)
                     0
                     0
                     update-file-entire
                     #:strict s?))]))

(define-syntax (expect-exn stx)
  (syntax-parse stx
    [(_ expr expected-first:str expected-rest:str ...
        (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     (define expect-list (syntax->list #'(expected-first expected-rest ...)))
     (define first #'expected-first)
     (define last-syn (if (null? (syntax->list #'(expected-rest ...)))
                         #'expected-first
                         (last expect-list)))
    (define src (syntax-source first))
    (define pos (or (syntax-position first) 0))
    (define span (- (+ (or (syntax-position last-syn) 0)
                        (or (syntax-span last-syn)
                            (string-length (syntax-e last-syn))))
                     pos))
     #`(run-expect-exn (lambda () expr)
                       (string-append #,@expect-list)
                       #,(and src (path->string src))
                       #,pos
                       #,span
                       #:strict s?)]
    [(_ expr (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     #'(run-expect-exn (lambda () expr) "" #f 0 0 #:strict s?)]))

