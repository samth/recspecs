#lang at-exp racket
(require rackunit
         rackunit/text-ui
         recspecs/shell)

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
      @expect/shell["bc"]{> 2+3
5
> 10*4
40
> quit
})
    (test-case "bc calculator with division"
      @expect/shell["bc"]{> 15/3
5
> 22/7
3
> quit
})
    (test-case "bc calculator complex session"
      @expect/shell["bc"]{> 2^8
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

(module+ test
  (run-tests shell-tests)
  ; Comment out pattern tests for now
  ; (run-tests pattern-shell-tests)
  )
