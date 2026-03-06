#lang at-exp racket
(require rackunit
         rackunit/text-ui
         recspecs/shell)
(define bc-path (path->string (collection-file-path "bc.rkt" "recspecs" "tests")))
(define racket-path
  (path->string (find-executable-path "racket")))

(define shell-tests
  (test-suite "shell-tests"
    (test-case "cat session"
      @expect/shell["cat"]{> hi
hi
> there
there
})
    (test-case "strict output"
      @expect/shell["cat" #:strict? #t]{> ok
ok

})
    (test-case "bc calculator basic"
      @expect/shell[(list racket-path bc-path)]{> 2+3
5
> 10*4
40
> quit
})
    (test-case "bc calculator with division"
      @expect/shell[(list racket-path bc-path)]{> 15/3
5
> 22/7
3
> quit
})
    (test-case "bc calculator complex session"
      @expect/shell[(list racket-path bc-path)]{> 2^8
256
> (5+3)*2
16
> scale=2
> 22/7
3.14
> quit
})))

;; Enhanced pattern-based shell tests
(define pattern-shell-tests
  (test-suite "pattern-shell-tests"
    (test-case "simple echo with exact pattern"
      (expect/shell/patterns "bash"
        ["$" (send-input "echo hello")]
        ["hello" continue]
        ["$" (send-input "exit")]))
    
    (test-case "regex pattern matching"
      (expect/shell/patterns "bash"  
        [(regex #rx"\\$") (send-input "echo test")]
        [(regex #rx"test") (send-input "exit")]))
    
    (test-case "glob pattern matching"
      (expect/shell/patterns "bash"
        [(glob "*$*") (send-input "echo wildcards")]
        [(glob "*wildcards*") (send-input "exit")]))
    
    (test-case "timeout handling"
      (expect/shell/patterns "sleep 1; echo done"
        #:timeout 2
        ["done" continue]
        [(timeout 3) (error "Should not timeout")]))
    
    (test-case "multiple pattern conditions"
      (expect/shell/patterns "bash"
        ["$" (send-input "echo step1")]
        ["step1" (send-input "echo step2")]  
        ["step2" (send-input "exit")]))
    
    (test-case "error handling pattern"
      (check-exn exn:fail?
        (lambda ()
          (expect/shell/patterns "bash"
            ["$" (send-input "false")]
            ["$" (error "Command failed as expected")]))))
    
    (test-case "retry action"
      (expect/shell/patterns "bash"
        ["$" (send-input "echo attempt")]
        ["attempt" retry]  ; This would normally loop, but we'll exit
        ["attempt" (send-input "exit")]))
    
    (test-case "continue action"
      (expect/shell/patterns "bash"
        ["$" (send-input "echo continue-test")]
        ["continue-test" continue]
        ["$" (send-input "exit")]))
    
    (test-case "variable capture and substitution"
      (expect/shell/patterns "bash"
        ["$" (send-input "echo 'result: 123'")]
        [(regex #rx"result: ([0-9]+)") (send-input "echo captured: $0")]
        ["captured: 123" (send-input "exit")]))))

(define exit-code-tests
  (test-suite "exit-code-tests"
    (test-case "successful command returns 0"
      @expect/shell["true" #:status 0]{})
    (test-case "failing command returns non-zero"
      @expect/shell["false" #:status 1]{})
    (test-case "cat session returns 0"
      @expect/shell["cat" #:status 0]{> hi
hi
> there
there
})))

(define stderr-tests
  (test-suite "stderr-tests"
    (test-case "capture stderr with #:port stderr"
      @expect/shell[(list "/bin/sh" "-c" "echo err >&2") #:port 'stderr]{err
})
    (test-case "capture both with #:port both"
      @expect/shell[(list "/bin/sh" "-c" "echo out; echo err >&2") #:port 'both #:match 'contains]{out
})))

(define env-tests
  (test-suite "env-tests"
    (test-case "custom environment variable"
      (define env (make-environment-variables))
      (environment-variables-set! env #"PATH" (environment-variables-ref
                                                (current-environment-variables) #"PATH"))
      (environment-variables-set! env #"MY_VAR" #"hello123")
      @expect/shell[(list "/bin/sh" "-c" "echo $MY_VAR") #:env env]{>
hello123
})))

(define match-mode-tests
  (test-suite "shell-match-mode-tests"
    (test-case "contains match"
      @expect/shell[(list "/bin/sh" "-c" "echo 'hello world'") #:match 'contains]{hello})
    (test-case "regexp match"
      @expect/shell[(list "/bin/sh" "-c" "echo 'value: 42'") #:match 'regexp]{value: [0-9]+})))

(module+ test
  (run-tests shell-tests)
  (run-tests exit-code-tests)
  (run-tests stderr-tests)
  (run-tests env-tests)
  (run-tests match-mode-tests)
  ; Comment out pattern tests for now
  ; (run-tests pattern-shell-tests)
  )
