#lang racket
;; Simple bc replacement for tests
(module+ main
  (define scale 0)

  ;; Tokenize an expression into numbers and operators
  (define (tokenize s)
    (regexp-match* #px"[0-9]+|[+*/^()-]" s))

  ;; Forward declarations for mutually recursive parsers
  (define parse-expr #f)
  (define parse-term #f)
  (define parse-power #f)
  (define parse-factor #f)

  (set! parse-factor
        (lambda (tokens)
          (match tokens
            [(cons "(" rest)
             (define-values (v rest2) (parse-expr rest))
             (unless (and rest2 (equal? (car rest2) ")"))
               (error 'bc "missing )"))
             (values v (cdr rest2))]
            [(cons n rest) (values (string->number n) rest)]
            [else (error 'bc "bad factor")])))

  (set! parse-power
        (lambda (tokens)
          (define-values (base rest) (parse-factor tokens))
          (match rest
            [(cons "^" r)
             (define-values (exp r2) (parse-power r))
             (values (expt base exp) r2)]
            [else (values base rest)])))

  (set! parse-term
        (lambda (tokens)
          (define-values (v rest) (parse-power tokens))
          (let loop ([v v]
                     [t rest])
            (match t
              [(cons "*" r)
               (define-values (rhs r2) (parse-power r))
               (loop (* v rhs) r2)]
              [(cons "/" r)
               (define-values (rhs r2) (parse-power r))
               (loop (/ v rhs) r2)]
              [else (values v t)]))))

  (set! parse-expr
        (lambda (tokens)
          (define-values (v rest) (parse-term tokens))
          (let loop ([v v]
                     [t rest])
            (match t
              [(cons "+" r)
               (define-values (rhs r2) (parse-term r))
               (loop (+ v rhs) r2)]
              [(cons "-" r)
               (define-values (rhs r2) (parse-term r))
               (loop (- v rhs) r2)]
              [else (values v t)]))))

  (define (format-result n)
    (cond
      [(integer? n)
       (printf "~a\n" n)
       (flush-output)]
      [else
       (if (> scale 0)
           (printf "~a\n" (real->decimal-string n scale))
           (printf "~a\n" (inexact->exact (truncate n))))
       (flush-output)]))

  (define (eval-line line)
    (cond
      [(regexp-match #px"^scale=([0-9]+)$" line)
       =>
       (lambda (m) (set! scale (string->number (cadr m))))]
      [else
       (define tokens (tokenize line))
       (define-values (result rest) (parse-expr tokens))
       (when rest
         (void))
       (format-result result)]))

  (let loop ()
    (define line (read-line))
    (cond
      [(eof-object? line) (void)]
      [(equal? line "quit") (void)]
      [else
       (with-handlers ([exn:fail? (lambda (_) (printf "error\n"))])
         (eval-line (string-trim line)))
       (loop)])))
