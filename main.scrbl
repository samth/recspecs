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
diff is printed to help understand the mismatch.

@defform[(expect expr expected-str)]{
Evaluates @racket[expr] and checks that the captured output is equal to
@racket[expected-str]. If they differ and @tt{RECSPECS_UPDATE} is set,
the expectation string in the source file is replaced with the new
value.  Otherwise the test case fails.
}

@racketblock[
  (require recspecs)
  (expect (displayln "hello") "hello\n")]

@defform[(expect-file expr path-str)]{
Reads the expectation from @racket[path-str] instead of embedding it in the
source. The file is replaced with new output when @tt{RECSPECS_UPDATE} is set.
}

@defform[(expect-exn expr expected-str)]{
Checks that @racket[expr] raises an exception whose message matches
@racket[expected-str]. The message is updated when update mode is enabled.
}


