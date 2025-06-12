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

For Emacs users, the accompanying @filepath{emacs/recspecs.el} file
provides @racketfont{recspecs-update-at-point}, which runs the current
file under @exec{racket-test} with those environment variables set for
the expectation at the cursor position.

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

@defform[(expect-file expr path-str)]{
Reads the expectation from @racket[path-str] instead of embedding it in the
source. The file is replaced with new output when @tt{RECSPECS_UPDATE} is set.
}

@defform[(expect-exn expr expected-str ...)]{
Checks that @racket[expr] raises an exception whose message matches the
concatenation of @racket[expected-str]s. The message is updated when
update mode is enabled.
}

@defparam[recspecs-output-filter filter (-> string? string?)]{
A parameter whose value is applied to captured output and exception
messages before they are compared against expectations.  Use this to
strip nondeterministic data.

@racketblock[
  (parameterize ([recspecs-output-filter
                  (lambda (s)
                    (regexp-replace #px"timestamp: \\d+" s "timestamp: <ts>"))])
    (expect (displayln "timestamp: 123") "timestamp: <ts>\n"))]
}


