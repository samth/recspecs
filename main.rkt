#lang racket/base

(require racket/file
         racket/port
         racket/list
         racket/string
         racket/vector
         racket/match
         racket/path
         rackunit
         (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/file))

(provide expect
         expect-file
         expect-exn
         expect-unreachable
         recspecs-verbose?
         recspecs-output-filter)

;; When enabled, expectation output is printed to the actual output
;; port as it is produced. The parameter defaults to #t when the
;; `RECSPECS_VERBOSE` environment variable is set.
(define recspecs-verbose? (make-parameter (and (getenv "RECSPECS_VERBOSE") #t)))

;; A procedure applied to the captured output before it is compared or
;; written back to a file. The parameter defaults to the identity function.
(define recspecs-output-filter (make-parameter (lambda (s) s)))

;; Normalize a string by trimming leading and trailing whitespace and removing
;; common indentation from all lines. This is used when comparing expectation
;; strings in non strict mode.
(define (normalize-string s)
  (define trimmed (string-trim s))
  (define lines (regexp-split #px"\r?\n" trimmed))
  (define non-empty-lines (filter (lambda (l) (regexp-match? #px"\\S" l)) lines))
  (define min-indent
    (if (null? non-empty-lines)
        0
        (apply min
               (map (lambda (l) (string-length (car (regexp-match #px"^[ \t]*" l))))
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
  ;; at `path` with `new-str`.  If the original source used a quoted
  ;; Racket string, keep using the printed string form.  Otherwise insert
  ;; the raw text.  This avoids introducing double quotes when updating
  ;; expectations written with the @-syntax from `#lang at-exp`.
  (define bs (file->bytes path))
  (define start (sub1 pos))
  (define before (subbytes bs 0 start))
  (define orig-bs (subbytes bs start (+ start span)))
  (define after (subbytes bs (+ start span)))
  (define orig (bytes->string/utf-8 orig-bs))
  ;; If the original text was written as a Racket string, it will start
  ;; and end with a double quote character (possibly surrounded by
  ;; whitespace).  Detect that so we know whether to emit the printed
  ;; representation or the raw text.
  (define (quoted-string? s)
    (let ([t (string-trim s)])
      (and (>= (string-length t) 2)
           (char=? (string-ref t 0) #\")
           (char=? (string-ref t (sub1 (string-length t))) #\"))))
  (define quoting? (quoted-string? orig))
  (define replacement
    (if quoting?
        (format "~s" new-str)
        new-str))
  (define new-bs (bytes-append before (string->bytes/utf-8 replacement) after))
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (write-bytes new-bs out))))

;; Replace empty braces at [pos, pos+span) with `{new-str}`
(define (update-file-empty path pos span new-str)
  (define bs (file->bytes path))
  (define start (sub1 pos))
  (define before (subbytes bs 0 start))
  (define snippet (bytes->string/utf-8 (subbytes bs start (+ start span))))
  (define after (subbytes bs (+ start span)))
  (define replaced (regexp-replace #px"\\{\\s*\\}\\s*$" snippet (format "{~a}" new-str)))
  (define new-bs (bytes-append before (string->bytes/utf-8 replaced) after))
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (write-bytes new-bs out))))

;; Replace the entire file at `path` with `new-str`.
(define (update-file-entire path _pos _span new-str)
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (display new-str out))))

;; Replace the form at [pos, pos+span) with `new-str`.
(define (update-file-rewrite path pos span new-str)
  (define bs (file->bytes path))
  (define start (sub1 pos))
  (define before (subbytes bs 0 start))
  (define after (subbytes bs (+ start span)))
  (define new-bs (bytes-append before (string->bytes/utf-8 new-str) after))
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (write-bytes new-bs out))))

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
      (vector-set! (vector-ref tbl (add1 i))
                   (add1 j)
                   (if (string=? (list-ref a-lines i) (list-ref b-lines j))
                       (add1 (vector-ref (vector-ref tbl i) j))
                       (max (vector-ref (vector-ref tbl i) (add1 j))
                            (vector-ref (vector-ref tbl (add1 i)) j))))))
  ;; backtrack
  (define diffs '())
  (let loop ([i m]
             [j n])
    (cond
      [(and (> i 0) (> j 0) (string=? (list-ref a-lines (sub1 i)) (list-ref b-lines (sub1 j))))
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
(define (pretty-diff expected actual #:color? [color? #t])
  (define diffs (lines-diff (string->lines expected) (string->lines actual)))
  (define (color c s)
    (if color?
        (string-append "\x1b[" c "m" s "\x1b[0m")
        s))
  (string-join (for/list ([d diffs])
                 (match d
                   [(cons 'same l) (string-append "  " l)]
                   [(cons 'add l) (color "32" (string-append "+ " l))]
                   [(cons 'del l) (color "31" (string-append "- " l))]))
               "\n"))

(define (run-expect thunk expected path pos span [update update-file] #:strict [strict? #f])
  ;; Returns a rackunit test that evaluates `thunk`, captures anything printed
  ;; to the current output port and compares it to `expected`. When update mode
  ;; is enabled and the values differ, the source file is rewritten instead of
  ;; failing the test.
  (define name
    (if path
        (format "~a:~a" path pos)
        "expect"))
  (test-case name
    (define out-str (open-output-string))
    (define base (current-output-port))
    (define raw
      (parameterize ([current-output-port (if (recspecs-verbose?)
                                              (combine-output base out-str)
                                              out-str)])
        (thunk)
        (get-output-string out-str)))
    (define actual ((recspecs-output-filter) raw))
    (define comparator
      (if strict?
          string=?
          (lambda (e a) (string=? (normalize-string a) (normalize-string e)))))
    (define equal? (comparator expected actual))
    (cond
      [(and path (update-mode? name) (not equal?))
       (update path pos span actual)
       (printf "Updated expectation in ~a\n" path)]
      [equal? (check comparator expected actual)]
      [else
       (define color? (and (terminal-port? (current-error-port)) (not (getenv "NO_COLOR"))))
       (displayln "Diff:" (current-error-port))
       (displayln (pretty-diff expected actual #:color? color?) (current-error-port))
       (check comparator expected actual)])))

(define (run-expect-exn thunk expected path pos span [update update-file] #:strict [strict? #f])
  (define name
    (if path
        (format "~a:~a" path pos)
        "expect-exn"))
  (test-case name
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (define raw (exn-message e))
            (define actual ((recspecs-output-filter) raw))
            (define comparator
              (if strict?
                  string=?
                  (lambda (e a) (string=? (normalize-string a) (normalize-string e)))))
            (define equal? (comparator expected actual))
            (cond
              [(and path (update-mode? name) (not equal?))
               (update path pos span actual)
               (printf "Updated expectation in ~a\n" path)]
              [equal? (check comparator expected actual)]
              [else
               (define color? (and (terminal-port? (current-error-port)) (not (getenv "NO_COLOR"))))
               (displayln "Diff:" (current-error-port))
               (displayln (pretty-diff expected actual #:color? color?) (current-error-port))
               (check comparator expected actual)]))])
      (begin
        (thunk)
        (fail "expected an exception")))))

;; Produce a test that fails when evaluated. In update mode the form in the
;; source file is replaced with the printed expression instead of failing.
(define (run-expect-unreachable expr-str path pos span)
  (define name
    (if path
        (format "~a:~a" path pos)
        "expect-unreachable"))
  (test-case name
    (cond
      [(and path (update-mode? name))
       (update-file-rewrite path pos span expr-str)
       (printf "Updated expectation in ~a\n" path)]
      [else (fail "unreachable expression evaluated")])))

(define-syntax (expect stx)
  (syntax-parse stx
    [(_ expr
        expected-first:str
        expected-rest:str ...
        (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     (define expect-list (syntax->list #'(expected-first expected-rest ...)))
     (define first #'expected-first)
     (define last-syn
       (if (null? (syntax->list #'(expected-rest ...)))
           #'expected-first
           (last expect-list)))
     (define src (syntax-source first))
     (define pos (or (syntax-position first) 0))
     (define span
       (- (+ (or (syntax-position last-syn) 0)
             (or (syntax-span last-syn) (string-length (syntax-e last-syn))))
          pos))
     #`(run-expect (lambda () expr)
                   (string-append #,@expect-list)
                   #,(and src (path->string src))
                   #,pos
                   #,span
                   #:strict s?)]
    [(_ expr (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     (define src (syntax-source stx))
     (define pos (syntax-position stx))
     (define span (syntax-span stx))
     (define snippet (and src pos span (substring (file->string src) (sub1 pos) (+ (sub1 pos) span))))
     (define has-empty? (and snippet (regexp-match? #px"\\{\\s*\\}\\s*$" snippet)))
     (if has-empty?
         #`(run-expect (lambda () expr)
                       ""
                       #,(path->string src)
                       #,pos
                       #,span
                       update-file-empty
                       #:strict s?)
         #'(run-expect (lambda () expr) "" #f 0 0 #:strict s?))]))

(define-syntax (expect-file stx)
  (syntax-parse stx
    [(_ expr path:str (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     #'(let* ([p path]
              [p (if (path? p)
                     p
                     (string->path p))])
         (run-expect (lambda () expr)
                     (call-with-input-file p port->string)
                     (path->string p)
                     0
                     0
                     update-file-entire
                     #:strict s?))]))

(define-syntax (expect-exn stx)
  (syntax-parse stx
    [(_ expr
        expected-first:str
        expected-rest:str ...
        (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     (define expect-list (syntax->list #'(expected-first expected-rest ...)))
     (define first #'expected-first)
     (define last-syn
       (if (null? (syntax->list #'(expected-rest ...)))
           #'expected-first
           (last expect-list)))
     (define src (syntax-source first))
     (define pos (or (syntax-position first) 0))
     (define span
       (- (+ (or (syntax-position last-syn) 0)
             (or (syntax-span last-syn) (string-length (syntax-e last-syn))))
          pos))
     #`(run-expect-exn (lambda () expr)
                       (string-append #,@expect-list)
                       #,(and src (path->string src))
                       #,pos
                       #,span
                       #:strict s?)]
    [(_ expr (~optional (~seq #:strict? s?:expr) #:defaults ([s? #'#f])))
     (define src (syntax-source stx))
     (define pos (syntax-position stx))
     (define span (syntax-span stx))
     (define snippet (and src pos span (substring (file->string src) (sub1 pos) (+ (sub1 pos) span))))
     (define has-empty? (and snippet (regexp-match? #px"\\{\\s*\\}\\s*$" snippet)))
     (if has-empty?
         #`(run-expect-exn (lambda () expr)
                           ""
                           #,(path->string src)
                           #,pos
                           #,span
                           update-file-empty
                           #:strict s?)
         #'(run-expect-exn (lambda () expr) "" #f 0 0 #:strict s?))]))

(define-syntax (expect-unreachable stx)
  (syntax-parse stx
    [(_ expr)
     (define src (syntax-source stx))
     (define pos (syntax-position stx))
     (define span (syntax-span stx))
     (define expr-str (format "~s" (syntax->datum #'expr)))
     #`(run-expect-unreachable #,expr-str #,(and src (path->string src)) #,pos #,span)]))
