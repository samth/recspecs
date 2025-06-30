#lang racket/base
(require racket/system
         racket/port
         racket/match
         racket/string
         racket/list
         racket/format
         racket/contract
         "main.rkt"
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     racket/list))

(provide expect/shell 
         expect/shell/patterns
         pattern-action pattern-exact pattern-regex pattern-glob 
         pattern-timeout pattern-eof
         action-send-text action-continue action-retry action-error action-proc
         shell-run-patterns
         match-pattern)

;; Pattern matching structures
(struct pattern-action (pattern action vars) #:transparent)
(struct pattern-exact (text) #:transparent)
(struct pattern-regex (regex) #:transparent)
(struct pattern-glob (pattern) #:transparent)
(struct pattern-timeout (seconds) #:transparent)
(struct pattern-eof () #:transparent)

;; Action types
(struct action-send-text (text) #:transparent)
(struct action-continue () #:transparent)
(struct action-retry () #:transparent)
(struct action-error (message) #:transparent)
(struct action-proc (proc) #:transparent)

;; Shell session state
(struct shell-session (in out pid err ctrl vars timeout) #:mutable #:transparent)

;; Pattern matching utilities
(define (match-pattern pattern text vars)
  "Returns (values matched? captured-vars new-text) for a pattern match"
  (match pattern
    [(pattern-exact expected)
     (if (string-contains? text expected)
         (values #t vars text)
         (values #f vars text))]
    [(pattern-regex rx)
     (define m (regexp-match rx text))
     (if m
         (let ([captured (cdr m)])
           (values #t (append vars captured) text))
         (values #f vars text))]
    [(pattern-glob pat)
     (if (string-glob-match? pat text)
         (values #t vars text)
         (values #f vars text))]
    [(pattern-timeout _) (values #f vars text)]
    [(pattern-eof) (values #f vars text)]))

(define (string-glob-match? pattern text)
  "Simple glob matching with * and ? wildcards"
  (regexp-match? (glob->regex pattern) text))

(define (glob->regex pattern)
  "Convert glob pattern to regex"
  (define escaped (regexp-replace* #rx"[.^$+{}()|\\[\\]\\\\]" pattern "\\\\&"))
  (define with-star (regexp-replace* #rx"\\*" escaped ".*"))
  (define with-question (regexp-replace* #rx"\\?" with-star "."))
  (pregexp with-question))

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

;; Enhanced shell runner with pattern matching
(define (shell-run-patterns cmd patterns #:timeout [default-timeout 30])
  "Run shell with pattern/action pairs for interactive control"
  (define session (start-shell-session cmd default-timeout))
  (define output-buffer "")
  
  (let loop ([remaining-patterns patterns]
             [vars '()])
    (cond
      [(null? remaining-patterns) 
       (close-shell-session session)]
      [else
       (define current-pattern (car remaining-patterns))
       (define rest-patterns (cdr remaining-patterns))
       
       ;; Read output and try to match patterns
       (define new-output (read-shell-output session))
       (set! output-buffer (string-append output-buffer new-output))
       (displayln new-output) ; Echo output
       
       ;; Try to match the pattern
       (define-values (matched? new-vars _) 
         (match-pattern (pattern-action-pattern current-pattern) 
                       output-buffer 
                       vars))
       
       (if matched?
           ;; Execute action and continue
           (let ([action-result (execute-action session 
                                              (pattern-action-action current-pattern) 
                                              new-vars)])
             (match action-result
               ['continue (loop rest-patterns new-vars)]
               ['retry (loop remaining-patterns vars)]
               [(list 'error msg) (error msg)]
               [_ (loop rest-patterns new-vars)]))
           ;; Pattern not matched, keep trying with same pattern
           (begin
             (sleep 0.1) ; Brief pause before retry
             (loop remaining-patterns vars)))])))

(define (start-shell-session cmd timeout)
  "Start a new shell session"
  (define-values (out in pid err ctrl)
    (apply values
           (if (list? cmd)
               (apply process* cmd)
               (process cmd))))
  (shell-session in out pid err ctrl '() timeout))

(define (close-shell-session session)
  "Clean up shell session"
  (close-output-port (shell-session-in session))
  ((shell-session-ctrl session) 'wait))

(define (read-shell-output session)
  "Read available output from shell session"
  (define out (shell-session-out session))
  (define result "")
  (let loop ()
    (if (char-ready? out)
        (let ([line (read-line out)])
          (if (eof-object? line)
              result
              (begin
                (set! result (string-append result line "\n"))
                (loop))))
        result)))

(define (execute-action session action vars)
  "Execute an action in the shell session"
  (match action
    [(action-send-text text)
     (define interpolated (interpolate-vars text vars))
     (display "> ")
     (displayln interpolated)
     (display interpolated (shell-session-in session))
     (newline (shell-session-in session))
     (flush-output (shell-session-in session))
     'continue]
    [(action-continue) 'continue]
    [(action-retry) 'retry]
    [(action-error msg) (list 'error msg)]
    [(action-proc proc) (proc session vars)]))

(define (interpolate-vars text vars)
  "Replace variable placeholders in text with captured values"
  (let loop ([result text] [i 0] [var-list vars])
    (if (null? var-list)
        result
        (let* ([placeholder (format "$~a" i)]
               [value (car var-list)]
               [new-result (string-replace result placeholder value)])
          (loop new-result (+ i 1) (cdr var-list))))))

(define (run-expect-shell cmd transcript path pos span #:strict [strict? #f])
  (run-expect (lambda () (shell-run cmd transcript)) transcript path pos span #:strict strict?))

(define-syntax (expect/shell stx)
  (syntax-parse stx
    [(_ cmd
	(~optional (~seq #:strict? s?) #:defaults ([s? #'#f]))
        expected-first:str
        expected-rest:str ...)
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

;; New enhanced expect/shell with pattern matching
(define-syntax (expect/shell/patterns stx)
  (syntax-parse stx
    [(_ cmd 
        (~optional (~seq #:timeout timeout-val) #:defaults ([timeout-val #'30]))
        (~optional (~seq #:strict? s?) #:defaults ([s? #'#f]))
        [pattern-spec action-spec] ...)
     #:declare cmd (expr/c #'any/c #:name "command")
     #:declare timeout-val (expr/c #'number? #:name "timeout value")
     #:declare s? (expr/c #'boolean? #:name "boolean value")
     
     (define pattern-actions
       (for/list ([p (syntax->list #'(pattern-spec ...))]
                  [a (syntax->list #'(action-spec ...))])
         #`(pattern-action #,(parse-pattern-syntax p)
                          #,(parse-action-syntax a)
                          '())))
     
     #`(run-expect 
        (lambda () 
          (shell-run-patterns cmd 
                             (list #,@pattern-actions)
                             #:timeout timeout-val))
        ""  ; No transcript for pattern-based mode
        #f 0 0 #:strict s?)]))

(define-for-syntax (parse-pattern-syntax pattern-stx)
  "Parse pattern syntax into pattern struct"
  (syntax-case pattern-stx (exact regex glob timeout eof)
    [pat 
     (string? (syntax-e #'pat))
     #'(pattern-exact pat)]
    [(exact pat)
     #'(pattern-exact pat)]
    [(regex pat)
     #'(pattern-regex pat)]
    [(glob pat)  
     #'(pattern-glob pat)]
    [(timeout seconds)
     #'(pattern-timeout seconds)]
    [eof
     #'(pattern-eof)]
    [_
     (raise-syntax-error 'expect/shell/patterns 
                         "invalid pattern syntax" 
                         pattern-stx)]))

(define-for-syntax (parse-action-syntax action-stx)
  "Parse action syntax into action struct"
  (syntax-case action-stx (send-input continue retry error)
    [(send-input text)
     #'(action-send-text text)]
    [continue
     #'(action-continue)]
    [retry
     #'(action-retry)]
    [(error msg)
     #'(action-error msg)]
    [proc
     #'(action-proc proc)]))
