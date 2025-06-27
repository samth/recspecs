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

@defproc[(make-expectation) expectation?]{Create a fresh expectation.}

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

