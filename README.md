# recspecs

`recspecs` provides a lightweight expect testing facility for Racket. It is
inspired by [Jane Street's `expect_test` for OCaml](https://github.com/janestreet/ppx_expect)
and the [`expect-test` crate](https://github.com/rust-analyzer/expect-test) for Rust.

Expect tests record the output of expressions directly in the source file.
Each `expect` form expands to a small RackUnit test that compares the
captured output against the recorded expectation. When the environment
variable `RECSPECS_UPDATE` is set, failing expectations are automatically
updated in the file instead of causing a failure.

Additional forms mirror features from the OCaml and Rust libraries:

* `expect-file` compares the output against the contents of a separate file
  and rewrites that file when updating.
* `expect-exn` checks that an expression raises an exception with a given
  message.
* `expect-unreachable` fails if the wrapped expression is evaluated.
* `expect/print` runs `expr`, prints the result with `print`, and compares
  the printed output.
* `expect/pretty` is like `expect/print` but uses `pretty-print`, so the
  expectation includes a trailing newline.
* `expect/shell` from `recspecs/shell` runs an external command and compares
  the session against a transcript. Lines beginning with `>` are sent to the
  command's input.
* All expectation forms accept multiple string arguments which are
  concatenated together. This is handy when using
  `#lang at-exp` for multi-line expectations.
* Setting the `RECSPECS_UPDATE_TEST` environment variable to a test case
  name limits updates to only that expectation.
* Set `RECSPECS_VERBOSE` or parameterize `recspecs-verbose?` to print
  captured output while tests run.
* Pass `#:port 'stderr` to capture output from `current-error-port` in
  `expect`, `expect-file`, `expect-exn`, or `capture-output`. Use `'both`
  to capture from both output ports at once.
* Use `capture-output` to run a thunk and return its printed output.

## Example

```racket
#lang racket
(require recspecs)

(expect
  (begin
    (displayln "hello")
    (displayln (+ 1 2)))
  "hello\n3\n")

(expect (display "oops" (current-error-port))
        "oops"
        #:port 'stderr)

(expect (begin
          (display "warn" (current-error-port))
          (display "out"))
        "warnout"
        #:port 'both)
```

Using @ expressions from `#lang at-exp` can make multi-line output
easier to write:

```racket
#lang at-exp racket
(require recspecs)

@expect[(begin (displayln "hello") (displayln (+ 1 2)))]{
hello
3}
```

Mark code that should not run with `expect-unreachable`:

```racket
(when #f
  (expect-unreachable (displayln "never")))
```

You can also capture output directly without an expectation:

```racket
(capture-output (lambda () (display "hi"))) ; => "hi"
(capture-output (lambda () (display "err" (current-error-port)))
               #:port 'stderr) ; => "err"
(capture-output (lambda ()
                  (display "warn" (current-error-port))
                  (display "out"))
               #:port 'both) ; => "warnout"
```

### Additional examples

Store expectations in a separate file with `expect-file`:

```racket
(expect-file
  (begin
    (displayln "hello")
    (displayln "world"))
  "expected.txt")
```

Check exception messages using `expect-exn`:

```racket
(expect-exn (raise-user-error "bad") "bad")
```

Automatically print a value before comparing:

```racket
(expect/print (+ 1 2) "3")
(expect/pretty '(1 2 3) "(1 2 3)\n")
@expect/shell["cat"]{
> hi
hi
> there
there
}
```

Transform output before comparison with `recspecs-output-filter`:

```racket
(parameterize ([recspecs-output-filter string-upcase])
  (expect (display "ok") "OK"))
```

Trim whitespace before checking the result:

```racket
(parameterize ([recspecs-output-filter string-trim])
  (expect (display "  hi  ") "hi"))
```

Remove digits entirely:

```racket
(parameterize ([recspecs-output-filter
                (lambda (s) (regexp-replace* #px"[0-9]+" s ""))])
  (expect (display "v1.2") "v."))
```

The library also exposes a mutable `expectation` value for recording
output programmatically. Use `with-expectation` to capture output into the
struct and call `commit-expectation!` or `skip-expectation!` to mark the
result:

```racket
(define e (make-expectation))
(with-expectation e (display "ok"))
(commit-expectation! e)
```

`with-expectation` can also wrap other recspecs forms. The recorded output is
available via @racket[expectation-out]:

```racket
(define log (make-expectation))
(with-expectation log
  (expect (display "hi") "hi"))
(commit-expectation! log)
(displayln (expectation-out log)) ; prints ""
```

Run the file with `raco test` (or any RackUnit runner) to execute the
expectations. If they fail and you want to update the saved output, set
`RECSPECS_UPDATE`:

```console
$ RECSPECS_UPDATE=1 raco test my-test.rkt
```
To update just one expectation, set `RECSPECS_UPDATE_TEST` to the name
shown for that test case:

```console
$ RECSPECS_UPDATE=1 RECSPECS_UPDATE_TEST=my-test.rkt:42 raco test my-test.rkt
```

Enable verbose output with:

```console
$ RECSPECS_VERBOSE=1 raco test my-test.rkt
```

### Emacs integration

The file `emacs/recspecs.el` defines a helper command
`recspecs-update-at-point`.  When called from a buffer visiting a Racket
file under `racket-mode`, it reruns that file with
`RECSPECS_UPDATE` enabled and sets `RECSPECS_UPDATE_TEST` to the
expectation at point so only that one is updated.  After the test
finishes, the buffer is automatically reverted to load any updated
expectations from disk.

To enable this, add `(load "<path-to-this-code>/emacs/recspecs.el")`
to your `.emacs` file.

## Status

This library is new but relatively-feature complete. However, it hasn't
been used in anger, so lots of things might change.

Almost all the code here was written by the OpenAI Codex tool

