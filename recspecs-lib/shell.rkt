#lang racket/base
(require racket/system
         racket/port
         racket/match
         racket/string
         racket/list
         "main.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     racket/list))

(provide expect/shell)

(define (parse-transcript str)
  (define lines (regexp-split #px"\r?\n" str))
  (when (and (pair? lines) (string=? (last lines) ""))
    (set! lines (drop-right lines 1)))
  (define entries '())
  (define current #f)
  (define outs '())
  (for ([l lines])
    (match (regexp-match #px"^>\\s*(.*)$" l)
      [(list _ cmd)
       (when current
         (set! entries (append entries (list (cons current outs)))))
       (set! current (string-trim cmd))
       (set! outs '())]
      [#f (set! outs (append outs (list l)))]))
  (when current
    (set! entries (append entries (list (cons current outs)))))
  entries)

(define (shell-run cmd transcript)
  (define steps (parse-transcript transcript))
  (define-values (out in pid err ctrl)
    (apply values
           (if (list? cmd)
               (apply process* cmd)
               (process cmd))))
  (for ([step steps])
    (define input (car step))
    (display "> ")
    (displayln input)
    (with-handlers ([exn:fail:filesystem? (lambda (e) (void))])
      (display input in)
      (newline in)
      (flush-output in))
    (for ([ign (in-list (cdr step))])
      (define line (read-line out))
      (unless (eof-object? line)
        (displayln line))))
  (close-output-port in)
  (let loop ()
    (define line (read-line out))
    (unless (eof-object? line)
      (displayln line)
      (loop)))
  (ctrl 'wait))

(define (run-expect-shell cmd transcript path pos span #:strict [strict? #f])
  (run-expect (lambda () (shell-run cmd transcript)) transcript path pos span #:strict strict?))

(define-syntax (expect/shell stx)
  (syntax-parse stx
    [(_ cmd
        expected-first:str
        expected-rest:str ...
        (~optional (~seq #:strict? s?) #:defaults ([s? #'#f])))
     #:declare cmd (expr/c #'any/c)
     #:declare s? (expr/c #'boolean?)
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
     #`(run-expect-shell cmd
                         (string-append #,@expect-list)
                         #,(and src (path->string src))
                         #,pos
                         #,span
                         #:strict s?)]
    [(_ cmd (~optional (~seq #:strict? s?) #:defaults ([s? #'#f])))
     #:declare cmd (expr/c #'any/c)
     #:declare s? (expr/c #'boolean?)
     (define src (syntax-source stx))
     (define pos (syntax-position stx))
     (define span (syntax-span stx))
     #'(run-expect-shell cmd "" #,(and src (path->string src)) #,pos #,span #:strict s?)]))
