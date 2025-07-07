#lang scribble/manual

@title{recspecs: Expect Testing for Racket}
@defmodule[recspecs]

The @racket[expect] form captures anything printed to the current output
port while evaluating an expression and compares it to a string literal
stored directly in the source file.  Each use expands to a
RackUnit @racket[test-case].  When the environment variable
@tt{RECSPECS_UPDATE} is set and the expectation does not match, the file
is rewritten with the new output instead of failing the test.  When
@tt{RECSPECS_UPDATE} is not set and the expectation fails, a colorized
diff is printed to help understand the mismatch. Updating can be
restricted to a single test case by setting
@tt{RECSPECS_UPDATE_TEST} to the name shown for that case.

Verbose mode can be enabled by setting @tt{RECSPECS_VERBOSE} or by
parameterizing @racket[recspecs-verbose?]. When enabled, captured
output is echoed to the real output port as it is produced.
For example:

@verbatim|{RECSPECS_VERBOSE=1 raco test my-test.rkt}|

For Emacs users, the accompanying @filepath{emacs/recspecs.el} file
provides @racketfont{recspecs-update-at-point}, which runs the current
file under @exec{racket-test} with those environment variables set for
the expectation at the cursor position. After the test finishes the
buffer is automatically reverted so that any updated expectations are
reloaded from disk.

Use @racket[#:port 'stderr] with @racket[expect], @racket[expect-file],
@racket[expect-exn], or @racket[capture-output] to record output written
to the current error port instead of the output port. Pass
@racket['both] to capture from both ports simultaneously.

Output can be transformed before it is compared by parameterizing
@racket[recspecs-output-filter]. The parameter holds a procedure that
receives the captured string and returns a new string used for the
comparison and when updating:

@racketblock[
  (parameterize ([recspecs-output-filter
                  (lambda (s) (regexp-replace* #px"[0-9]+" s ""))])
    (expect (display "v1.2") "v."))]
@racketblock[
  (parameterize ([recspecs-output-filter string-upcase])
    (expect (display "ok") "OK"))]
@racketblock[
  (parameterize ([recspecs-output-filter string-trim])
    (expect (display "  hi  ") "hi"))]

The thunk that performs the test is executed via the procedure stored in
@racket[recspecs-runner].  The default simply calls the thunk, but it can
be replaced to control the runtime context. For example, limit memory
usage with a new custodian and redirect the error port:

@racketblock[
  (parameterize ([recspecs-runner
                  (lambda (th)
                    (call-in-nested-thread
                     (lambda ()
                       (custodian-limit-memory (current-custodian) (* 1024 1024))
                       (parameterize ([current-error-port (current-output-port)])
                         (th)))))])
    (expect (begin
              (display "oops" (current-error-port))
              (make-bytes (* 2 1024 1024)))
            "oops"))]

@defform[(expect expr expected-str ...)]{
Evaluates @racket[expr] and checks that the captured output is equal to
the concatenation of @racket[expected-str]s. If they differ and
@tt{RECSPECS_UPDATE} is set, the expectation string in the source file
is replaced with the new value.  Otherwise the test case fails.
}

@racketblock[
  (require recspecs)
  (expect (displayln "hello") "hello\n")]

It can be convenient to use @racketmodname[at-exp] for multi-line
expectations:

@racketblock[#:lang at-exp racket
  (require recspecs)

  @expect[(begin (displayln "hello") (displayln (+ 1 2)))]{
  hello
  3}]

@defform[(expect/print expr expected-str ...)]{
Like @racket[expect], but the result of @racket[expr] is printed with
@racket[print] before comparison.  This is shorthand for
@racket[(expect (print expr) expected-str ...)].
}

@defform[(expect/pretty expr expected-str ...)]{
Like @racket[expect/print], but uses @racket[pretty-print] to output the
result.  The newline produced by @racket[pretty-print] is included in the
expectation.
}

@defform[(expect-file expr path-str)]{
Reads the expectation from @racket[path-str] instead of embedding it in the
source. The file is replaced with new output when @tt{RECSPECS_UPDATE} is set.
}
@racketblock[
  (expect-file
    (begin
      (displayln "hello")
      (displayln "world"))
    "expected.txt")]

@defform[(expect-exn expr expected-str ...)]{
Checks that @racket[expr] raises an exception whose message matches the
concatenation of @racket[expected-str]s. The message is updated when
update mode is enabled.
}
@racketblock[
  (expect-exn (raise-user-error "bad")
              "bad")]

@defform[(expect-unreachable expr)]{
Fails the enclosing test if @racket[expr] evaluates. When update mode is
enabled, the form is replaced with @racket[expr] in the source instead of
failing.
}
@racketblock[
  (when #f
    (expect-unreachable (displayln "never")))]

@defproc[(capture-output [thunk (-> any/c)] [#:port port any/c 'stdout]) string?]{
Runs @racket[thunk] and returns everything printed to the selected port(s).
When @racket[port] is @racket['stderr], the current error port is captured
instead of the output port. Pass @racket['both] to capture from both ports.
When @racket[recspecs-verbose?] is true, the output is also echoed to the
original port(s).

@racketblock[(capture-output (lambda () (display "hi")))]
@racketblock[(capture-output (lambda () (display "err" (current-error-port)))
            #:port 'stderr)]
@racketblock[(capture-output (lambda ()
              (display "warn" (current-error-port))
              (display "out"))
            #:port 'both)]
}

@defstruct[expectation ([out string?]
                        [committed? boolean?]
                        [skip? boolean?])]{
Represents recorded output that can be committed or skipped. The
structure is mutable so repeated @racket[with-expectation] blocks can
append to @racket[out].}


@defproc[(commit-expectation! [e expectation?]) void?]{Mark @racket[e] as committed.}

@defproc[(reset-expectation! [e expectation?]) void?]{Reset the output and flags of @racket[e].}

@defproc[(skip-expectation! [e expectation?]) void?]{Mark @racket[e] as skipped.}

@defform[(with-expectation e expr ...)]{
Evaluates the @racket[expr]s and appends anything printed to
@racket[e]'s @racket[out] field.}

You can wrap any of the expectation forms with @racket[with-expectation] and
access the captured output with @racket[expectation-out]:

@racketblock[
  (define log (make-expectation))
  (with-expectation log
    (expect (display "hi") "hi"))
  (commit-expectation! log)
  (displayln (expectation-out log))]


@defproc[(run-expect
          [thunk (-> any/c)]
          [expected string?]
          [path (or/c path-string? #f)]
          [pos exact-nonnegative-integer?]
          [span exact-nonnegative-integer?]
          [#:strict strict? boolean? #f]
          [#:port port (symbols 'stdout 'stderr 'both) 'stdout])
         void?]{
Runs @racket[thunk] and checks that the captured output matches
@racket[expected].  The @racket[path], @racket[pos] and @racket[span]
identify the source location used when updating.
}

@defproc[(run-expect-exn
          [thunk (-> any/c)]
          [expected string?]
          [path (or/c path-string? #f)]
          [pos exact-nonnegative-integer?]
          [span exact-nonnegative-integer?]
          [#:strict strict? boolean? #f]
          [#:port port (symbols 'stdout 'stderr 'both) 'stdout])
         void?]{
Like @racket[run-expect] but expects @racket[thunk] to raise an
exception whose message matches @racket[expected].
}

@defproc[(update-file-entire
          [path path-string?]
          [pos exact-nonnegative-integer?]
          [span exact-nonnegative-integer?]
          [new-str string?])
         void?]{
Replace the entire file at @racket[path] with @racket[new-str].
}

@section{Shell Commands}
@defmodule[recspecs/shell]

The @racket[recspecs/shell] module provides tools for testing interactive shell 
commands, inspired by the Unix @exec{expect} tool. It supports both simple transcript-based 
testing and advanced pattern-based automation.

@subsection{Basic Shell Testing}

@defform[(expect/shell cmd-expr expected-str ...)]{
Run @racket[cmd-expr] as a subprocess and compare the interaction
against @racket[expected-str ...].  Lines in the expectation that begin
with @litchar{>} are sent to the process as input (without the prompt).
The command's responses are captured and the full transcript is checked
against the expectation.
}
@racketblock[
  (require recspecs/shell)
  @expect/shell["cat"]{
  > hi
  hi
  > there
  there
  }
]

@subsection{Pattern-Based Shell Automation}

For complex interactive scenarios, @racket[expect/shell/patterns] provides a 
declarative pattern-matching approach similar to the Unix @exec{expect} tool.

@defform[(expect/shell/patterns cmd-expr option ... [pattern action] ...)
         #:grammar
         ([option (code:line #:timeout timeout-expr)
                  (code:line #:strict? strict?-expr)]
          [pattern string-expr
                   (code:line (exact string-expr))
                   (code:line (regex regex-expr))
                   (code:line (glob glob-string-expr))
                   (code:line (timeout seconds-expr))
                   (code:line eof)]
          [action (code:line (send-input text-expr))
                  (code:line continue)
                  (code:line retry)
                  (code:line (error message-expr))
                  procedure-expr])]{

Runs @racket[cmd-expr] as an interactive subprocess and processes output using 
pattern/action pairs. Each pattern is matched against the accumulated output, and 
when matched, the corresponding action is executed.

@bold{Options:}
@itemlist[
@item{@racket[#:timeout] — Sets the default timeout in seconds for the entire session (default: 30)}
@item{@racket[#:strict?] — Controls whether whitespace normalization is applied (default: @racket[#f])}
]

@bold{Patterns:}
@itemlist[
@item{@racket[string-expr] or @racket[(exact string-expr)] — Exact string matching}
@item{@racket[(regex regex-expr)] — Regular expression matching with capture group support}
@item{@racket[(glob glob-string-expr)] — Glob pattern matching with @litchar{*} and @litchar{?} wildcards}
@item{@racket[(timeout seconds-expr)] — Matches when the specified timeout is reached}
@item{@racket[eof] — Matches when the process terminates}
]

@bold{Actions:}
@itemlist[
@item{@racket[(send-input text-expr)] — Send text as input to the process}
@item{@racket[continue] — Proceed to the next pattern}
@item{@racket[retry] — Retry the current pattern}
@item{@racket[(error message-expr)] — Raise an error with the given message}
@item{@racket[procedure-expr] — Call a custom procedure with session and variables}
]
}

@bold{Examples:}

Simple command interaction:
@racketblock[
  (expect/shell/patterns "bash"
    ["$" (send-input "echo hello")]
    ["hello" (send-input "exit")])]

Using regex patterns with timeout:
@racketblock[
  (expect/shell/patterns "slow-server" #:timeout 60
    [(regex #rx"Server started on port ([0-9]+)") 
     (send-input "connect")]
    [(timeout 30) (error "Server startup timeout")]
    ["Connected" continue])]

Glob patterns and error handling:
@racketblock[
  (expect/shell/patterns "deployment-script"
    [(glob "*$ ") (send-input "deploy app")]
    [(glob "*Success*") continue]
    [(glob "*Error*") (error "Deployment failed")]
    [(glob "*Warning*") continue])]

Variable capture and substitution:
@racketblock[
  (expect/shell/patterns "bash"
    ["$" (send-input "echo 'port: 8080'")]
    [(regex #rx"port: ([0-9]+)") (send-input "connect $0")]
    ["connected" (send-input "exit")])]

@subsection{Advanced Pattern Features}

@subsubsection{Variable Capture}

When using @racket[(regex regex-expr)] patterns, capture groups are automatically 
extracted and made available for variable substitution in subsequent actions. 
Variables are referenced as @racket[$0], @racket[$1], @racket[$2], etc., where 
@racket[$0] is the first capture group.

@racketblock[
  (expect/shell/patterns "date"
    [(regex #rx"([A-Z][a-z]+) ([0-9]+)") 
     (send-input "echo Month: $0, Day: $1")])]

@subsubsection{Flow Control}

@itemlist[
@item{@racket[continue] — Advances to the next pattern in the sequence}
@item{@racket[retry] — Repeats the current pattern (useful for polling)}
@item{@racket[(error msg)] — Terminates with a controlled error}
]

@racketblock[
  (expect/shell/patterns "build-system"
    ["Building..." continue]
    [(regex #rx"Progress: ([0-9]+)%") retry]  ; Keep polling
    ["Build complete" continue]
    [(glob "*failed*") (error "Build failed")])]

@subsubsection{Timeout Handling}

Individual patterns can specify timeouts, and a global session timeout can be set:

@racketblock[
  (expect/shell/patterns "long-running-process" #:timeout 300
    ["Starting..." continue]
    [(timeout 60) (send-input "status")]  ; Check status after 1 minute
    ["Progress: 100%" continue]
    [(timeout 300) (error "Process timeout")])]

@subsubsection{Custom Actions}

For complex logic, actions can be procedures that receive the session state and captured variables:

@racketblock[
  (define (analyze-output session vars)
    (if (> (length vars) 0)
        (printf "Captured: ~a~n" (car vars))
        (printf "No captures~n"))
    'continue)

  (expect/shell/patterns "analyzer"
    [(regex #rx"Result: (.+)") analyze-output])]

@subsection{Pattern Matching Reference}

@defproc[(match-pattern [pattern pattern?] [text string?] [vars list?]) 
         (values boolean? list? string?)]{
Tests whether @racket[pattern] matches @racket[text]. Returns three values:
whether the pattern matched, updated variable list with any captures, and the text.

This function underlies the pattern matching in @racket[expect/shell/patterns] and 
can be used directly for testing pattern logic.
}

@racketblock[
  (define-values (matched? vars text)
    (match-pattern (pattern-regex #rx"port: ([0-9]+)") "port: 8080" '()))
  ; matched? => #t
  ; vars => '("8080")
]

@subsection{Error Handling and Debugging}

When patterns fail to match or timeouts occur, @racket[expect/shell/patterns] provides 
detailed error messages including:

@itemlist[
@item{The accumulated output at the time of failure}
@item{The pattern that was being matched}
@item{Suggestions for common issues}
]

For debugging complex interactions, enable verbose mode with @racket[recspecs-verbose?] 
or the @tt{RECSPECS_VERBOSE} environment variable to see real-time output.

@subsection{Migration from Basic Shell Testing}

Existing @racket[expect/shell] tests can be gradually migrated to the pattern-based 
approach for enhanced functionality:

@racketblock[
  ; Before: transcript-based
  @expect/shell["interactive-app"]{
  > start
  Ready
  > process data.txt
  Processing...
  Done
  > quit
  }

  ; After: pattern-based
  (expect/shell/patterns "interactive-app"
    ["$" (send-input "start")]
    ["Ready" (send-input "process data.txt")]
    [(glob "*Processing*") continue]
    ["Done" (send-input "quit")])]

@subsection{Pattern and Action Structures}

The pattern-based shell automation is built on the following structures, which can be 
used directly for advanced scenarios:

@defstruct[pattern-action ([pattern pattern?] [action action?] [vars list?])]{
Combines a pattern with its corresponding action. The @racket[vars] field stores 
captured variables from previous pattern matches.
}

@defstruct[pattern-exact ([text string?])]{
Matches exact string content within the output.
}

@defstruct[pattern-regex ([regex regexp?])]{
Matches using regular expressions and captures groups for variable substitution.
}

@defstruct[pattern-glob ([pattern string?])]{
Matches using glob patterns with @litchar{*} and @litchar{?} wildcards.
}

@defstruct[pattern-timeout ([seconds number?])]{
Triggers when the specified number of seconds have elapsed.
}

@defstruct[pattern-eof ()]{
Triggers when the subprocess terminates or reaches end-of-file.
}

@defstruct[action-send-text ([text string?])]{
Sends the specified text to the subprocess as input.
}

@defstruct[action-continue ()]{
Proceeds to the next pattern in the sequence.
}

@defstruct[action-retry ()]{
Retries the current pattern without advancing.
}

@defstruct[action-error ([message string?])]{
Raises an error with the specified message.
}

@defstruct[action-proc ([proc procedure?])]{
Executes a custom procedure with signature @racket[(-> shell-session? list? symbol?)].
}

@defproc[(shell-run-patterns [cmd (or/c string? (listof string?))] 
                             [patterns (listof pattern-action?)] 
                             [#:timeout timeout number? 30]) 
         void?]{
Low-level function that runs the pattern-based shell interaction. This function 
underlies @racket[expect/shell/patterns] and can be used for programmatic control.
}

@racketblock[
  (shell-run-patterns "bash"
    (list (pattern-action (pattern-exact "$") 
                         (action-send-text "echo test") 
                         '())
          (pattern-action (pattern-exact "test")
                         (action-send-text "exit")
                         '())))]

@subsection{Best Practices}

@itemlist[
@item{Use specific patterns to avoid false matches: prefer @racket[(exact "$ ")] over @racket["$"]}
@item{Include timeout patterns for long-running operations}
@item{Use @racket[continue] judiciously to handle intermediate output}
@item{Capture important values with regex patterns for reuse}
@item{Test pattern logic in isolation using @racket[match-pattern]}
@item{Enable verbose mode during development for better visibility}
@item{Consider the order of patterns carefully: more specific patterns should come before general ones}
@item{Use @racket[eof] patterns to handle unexpected process termination gracefully}
]
