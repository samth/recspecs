#lang scribble/manual

@title{recspecs: Expect Testing for Racket}
@defmodule[recspecs]

@section{Getting Started}

Recspecs is useful when the easiest way to check a program is to look at
what it prints. Instead of writing assertions for every small value, you
run the code and keep the expected transcript next to the code that
produced it.

A minimal test file looks like this:

@racketblock[
  (require recspecs)

  (expect (displayln "hello")
          "hello\n")]

The @racket[expect] form captures anything printed to the current output
port while @racket[expr] runs. It compares that captured output to the
expected string in the source file, and each @racket[expect] expands to a
RackUnit @racket[test-case]. You can run the file with @exec{raco test}:

@verbatim|{raco test hello-test.rkt}|

@section[#:tag "at-exp"]{Writing Readable Expectations with @tt{#lang at-exp racket}}

Most expect tests are easiest to read with Racket's @racketmodname[at-exp]
reader. Start the file with @racketfont{#lang at-exp racket} instead of plain
@racketfont{#lang racket}, then put @litchar|{@}| before @racket[expect]. The
expression to run goes in square brackets, and the expected output goes in
braces:

@racketblock[#:lang at-exp racket
  (require recspecs)

  @expect[(displayln "hello")]{
  hello
  }]

The @litchar|{@}| form is just reader syntax for an ordinary function or
macro call. The example above is equivalent to writing an @racket[expect]
form with a string argument, but the expected output can be written as the
text you want to see instead of as a string with @racket["\\n"] escapes.

Here is a slightly more realistic example that checks a report:

@racketblock[#:lang at-exp racket
  (require recspecs)

  (define (print-shopping-list items)
    (for ([item items]
          [n (in-naturals 1)])
      (printf "~a. ~a\n" n item)))

  @expect[(print-shopping-list '("apples" "bread" "coffee"))]{
  1. apples
  2. bread
  3. coffee
  }]

Use this style when the expected output has more than one line or when the
literal output is clearer than a Racket string. For one-line output, a
plain string is still fine.

@section{Updating Recorded Output}

When the output intentionally changes, rerun the test with
@tt{RECSPECS_UPDATE} set. If an expectation does not match, recspecs
rewrites the expectation in the source file with the new output instead of
failing the test:

@verbatim|{RECSPECS_UPDATE=1 raco test shopping-list-test.rkt}|

After reviewing the rewritten file, run the tests normally again. Updating
can be restricted to a single test case by setting @tt{RECSPECS_UPDATE_TEST}
to the name shown for that case.

@section{Common First Use Cases}

@subsection{Testing Printed Output}

Use @racket[expect] when the behavior you care about is what the code
prints:

@racketblock[#:lang at-exp racket
  (require recspecs)

  (define (greet name)
    (printf "Hello, ~a!\n" name))

  @expect[(greet "Ada")]{
  Hello, Ada!
  }]

@subsection{Testing Printed Values}

Use @racket[expect/print] when you want to test the value an expression
returns. Recspecs prints the value with @racket[print] before comparing it:

@racketblock[#:lang at-exp racket
  (require recspecs)

  @expect/print[(map string-upcase '("red" "blue"))]{
  '("RED" "BLUE")
  }]

Use @racket[expect/pretty] for data that is easier to inspect with
@racket[pretty-print]:

@racketblock[#:lang at-exp racket
  (require recspecs)

  @expect/pretty['(shopping-list
                  (item "apples" #:qty 2 #:aisle "produce")
                  (item "bread" #:qty 1 #:aisle "bakery")
                  (item "coffee" #:qty 1 #:aisle "dry goods"))]{
  '(shopping-list
    (item "apples" #:qty 2 #:aisle "produce")
    (item "bread" #:qty 1 #:aisle "bakery")
    (item "coffee" #:qty 1 #:aisle "dry goods"))
  }]

@subsection{Testing Error Messages}

Use @racket[expect-exn] when the expected behavior is an exception:

@racketblock[#:lang at-exp racket
  (require recspecs)

  (define (parse-port n)
    (unless (and (integer? n) (<= 0 n 65535))
      (raise-user-error 'parse-port "expected an integer from 0 to 65535"))
    n)

  @expect-exn[(parse-port 70000)]{
  parse-port: expected an integer from 0 to 65535
  }]

@subsection{Capturing Output Without an Expectation}

Use @racket[capture-output] when you need the printed text as a string for
some other assertion or helper:

@racketblock[
  (require recspecs rackunit)

  (define out
    (capture-output
     (lambda ()
       (display "ready"))))

  (check-equal? out "ready")]

@section{Daily Workflow and Options}

Verbose mode can be enabled by setting @tt{RECSPECS_VERBOSE} or by
parameterizing @racket[recspecs-verbose?]. When enabled, captured output is
echoed to the real output port as it is produced:

@verbatim|{RECSPECS_VERBOSE=1 raco test my-test.rkt}|

For Emacs users, the accompanying @filepath{emacs/recspecs.el} file
provides @racketfont{recspecs-update-at-point}, which runs the current file
under @exec{racket-test} with the update environment variables set for the
expectation at the cursor position. After the test finishes the buffer is
automatically reverted so that any updated expectations are reloaded from
disk.

Use @racket[#:port] @racket['stderr] with @racket[expect], @racket[expect-file], or
@racket[capture-output] to record output written to the current error port
instead of the output port. Pass @racket['both] to capture from both ports
simultaneously:

@racketblock[#:lang at-exp racket
  (require recspecs)

  @expect[(display "oops" (current-error-port))
          #:port 'stderr]{
  oops
  }]

Output can be transformed before it is compared by parameterizing
@racket[recspecs-output-filter]. The parameter holds a procedure that
receives the captured string and returns a new string used for comparison
and updating. For example, trim incidental surrounding whitespace:

@racketblock[
  (parameterize ([recspecs-output-filter string-trim])
    (expect (display "  hi  ") "hi"))]

The thunk that performs the test is executed via the procedure stored in
@racket[recspecs-runner]. The default simply calls the thunk, but advanced
tests can replace it to control the runtime context. For example, you can
limit memory usage with a new custodian and redirect the error port:

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


@section{Reference}
@defform[(expect expr expected-str ...
                 [#:strict? strict?-expr #f]
                 [#:port port-expr 'stdout]
                 [#:match match-expr 'equal])]{
Evaluates @racket[expr] and checks that the captured output is equal to
the concatenation of @racket[expected-str]s.

@racket[#:strict?] controls whitespace comparison. When false, comparison
ignores surrounding whitespace and common indentation. When true, comparison
uses exact string equality. @racket[#:port] accepts @racket['stdout],
@racket['stderr], or @racket['both]. @racket[#:match] accepts
@racket['equal], @racket['contains], or @racket['regexp].

If the expectation differs and @tt{RECSPECS_UPDATE} is set, the expectation
string in the source file is replaced with the new value. Otherwise the test
case fails. Update mode is skipped for @racket['contains] and
@racket['regexp] because recspecs cannot infer the intended substring or
regular expression from the actual output.

@racketblock[
  (expect (display "hello world") "hello" #:match 'contains)
  (expect (display "value: 42") "value: [0-9]+" #:match 'regexp)]
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

@defform[(expect/print expr expected-str ...
                       [#:strict? strict?-expr #f]
                       [#:port port-expr 'stdout]
                       [#:match match-expr 'equal])]{
Like @racket[expect], but the result of @racket[expr] is printed with
@racket[print] before comparison. This is shorthand for
@racket[(expect (print expr) expected-str ...)]. It accepts the same
@racket[#:strict?], @racket[#:port], and @racket[#:match] keywords as
@racket[expect], with defaults shown in the form above.
}

@defform[(expect/pretty expr expected-str ...
                        [#:strict? strict?-expr #f]
                        [#:port port-expr 'stdout]
                        [#:match match-expr 'equal])]{
Like @racket[expect/print], but uses @racket[pretty-print] to output the
result. The newline produced by @racket[pretty-print] is included in the
expectation. It accepts the same @racket[#:strict?], @racket[#:port], and
@racket[#:match] keywords as @racket[expect], with defaults shown in the
form above.
}

@defform[(expect-file expr path-str
                       [#:strict? strict?-expr #f]
                       [#:port port-expr 'stdout])]{
Reads the expectation from @racket[path-str] instead of embedding it in the
source. The file is replaced with new output when @tt{RECSPECS_UPDATE} is set.
@racket[#:port] accepts @racket['stdout], @racket['stderr], or
@racket['both].
}
@racketblock[
  (expect-file
    (begin
      (displayln "hello")
      (displayln "world"))
    "expected.txt")]

@defform[(expect-exn expr expected-str ...
                      [#:strict? strict?-expr #f]
                      [#:port port-expr 'stdout])]{
Checks that @racket[expr] raises an exception whose message matches the
concatenation of @racket[expected-str]s. The message is updated when
update mode is enabled. @racket[#:port] is accepted for consistency with
the other expectation forms; @racket[expect-exn] compares the exception
message rather than captured output.
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

@defproc[(capture-output [thunk (-> any/c)] [#:port port (symbols 'stdout 'stderr 'both) 'stdout]) string?]{
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

@defproc[(capture-output/split [thunk (-> any/c)]) (values string? string?)]{
Runs @racket[thunk] and returns two values: the stdout output and the stderr
output as separate strings. When @racket[recspecs-verbose?] is true, both
streams are echoed to their original ports.

@racketblock[
  (define-values (out err)
    (capture-output/split
      (lambda ()
        (display "normal output")
        (display "error output" (current-error-port)))))
  out   ; => "normal output"
  err   ; => "error output"
]
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

@defform[(with-expectation e [#:port port-expr 'stdout] expr ...)]{
Evaluates the @racket[expr]s and appends anything printed to
@racket[e]'s @racket[out] field. @racket[#:port] accepts @racket['stdout],
@racket['stderr], or @racket['both].}

The recorded output is available with @racket[expectation-out]:

@racketblock[
  (define log (make-expectation))
  (with-expectation log
    (display "hi"))
  (commit-expectation! log)
  (displayln (expectation-out log))]


@defproc[(run-expect
          [thunk (-> any/c)]
          [expected string?]
          [path (or/c path-string? #f)]
          [pos exact-nonnegative-integer?]
          [span exact-nonnegative-integer?]
          [#:strict strict? boolean? #f]
          [#:port port (symbols 'stdout 'stderr 'both) 'stdout]
          [#:status status (or/c #f exact-integer?) #f]
          [#:match match-mode (symbols 'equal 'contains 'regexp) 'equal])
         void?]{
Runs @racket[thunk] and checks that the captured output matches
@racket[expected].  The @racket[path], @racket[pos] and @racket[span]
identify the source location used when updating.

When @racket[status] is not @racket[#f], the return value of
@racket[thunk] is also checked against @racket[status] using
@racket[check-equal?].

The @racket[match-mode] controls how the comparison is performed:
@itemlist[
@item{@racket['equal] (default) — exact string equality (modulo whitespace normalization when not strict)}
@item{@racket['contains] — passes when the actual output contains @racket[expected] as a substring}
@item{@racket['regexp] — passes when @racket[expected] matches the actual output as a regular expression}
]

Update mode is skipped for non-@racket['equal] match modes since
substring and regexp patterns cannot be auto-derived from output.
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
exception whose message matches @racket[expected]. The @racket[#:port]
keyword is accepted for consistency with expectation forms, but the
comparison is against the exception message.
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

The @racket[recspecs/shell] module provides tools for testing interactive
shell commands, inspired by the Unix @exec{expect} tool. Use
@racket[expect/shell] for transcript-based tests. The module also exposes
experimental pattern-matching helpers for lower-level interactive control.

@subsection{Basic Shell Testing}

@defform[(expect/shell cmd-expr
                         [#:strict? strict?-expr #f]
                         [#:status status-expr #f]
                         [#:port port-expr 'stdout]
                         [#:env env-expr #f]
                         [#:match match-expr 'equal]
                         expected-str ...)]{
Run @racket[cmd-expr] as a subprocess and compare the interaction
against @racket[expected-str ...]. Lines in the expectation that begin
with @litchar{>} are sent to the process as input (without the prompt).
The command's responses are captured and the full transcript is checked
against the expectation.

@racket[#:status] accepts an exact integer, such as @racket[0], to check
the subprocess exit code. @racket[#:port] accepts @racket['stdout],
@racket['stderr], or @racket['both]. @racket[#:env] accepts an
@racket[environment-variables?] value. @racket[#:match] accepts
@racket['equal], @racket['contains], or @racket['regexp].
}
@racketblock[#:lang at-exp racket
  (require recspecs/shell)
  @expect/shell["cat"]{
  > hi
  hi
  > there
  there
  }
]

@subsection{Experimental Pattern Helpers}

The pattern interface is experimental. Prefer @racket[expect/shell] for tests
that can be represented as a transcript. The lower-level pattern helpers are
useful for testing pattern parsing and for experimenting with interactive
control, but the current implementation does not implement timeout or EOF
pattern matching, and a pattern that never matches can block.

@racket[expect/shell/patterns] accepts exact, regexp, and glob patterns plus
actions that send input, continue, retry, raise an error, or call a custom
procedure:

@defform[(expect/shell/patterns cmd-expr
                                  [#:strict? strict?-expr #f]
                                  [pattern action] ...)
         #:grammar
         ([pattern string-expr
                   (code:line (exact string-expr))
                   (code:line (regex regex-expr))
                   (code:line (glob glob-string-expr))]
          [action (code:line (send-input text-expr))
                  (code:line continue)
                  (code:line retry)
                  (code:line (error message-expr))
                  procedure-expr])]{

Runs @racket[cmd-expr] as an interactive subprocess and processes
pattern/action pairs in order. Each pattern is matched against accumulated
output; when it matches, the action is executed. Regex captures are available
for substitution in @racket[(send-input text-expr)] as @racket[$0],
@racket[$1], and so on.
}

@subsection{Pattern Matching Reference}

@defproc[(match-pattern [pattern pattern?] [text string?] [vars list?]) 
         (values boolean? list? string?)]{
Tests whether @racket[pattern] matches @racket[text]. Returns three values:
whether the pattern matched, updated variable list with any captures, and the text.

This function underlies the pattern matching in @racket[expect/shell/patterns]
and can be used directly for testing pattern logic.
}

@racketblock[
  (define-values (matched? vars text)
    (match-pattern (pattern-regex #rx"port: ([0-9]+)") "port: 8080" '()))
  ; matched? => #t
  ; vars => '("8080")
]

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
@item{Use @racket[continue] judiciously to handle intermediate output}
@item{Capture important values with regex patterns for reuse}
@item{Test pattern logic in isolation using @racket[match-pattern]}
@item{Enable verbose mode during development for better visibility}
@item{Consider the order of patterns carefully: more specific patterns should come before general ones}
]
