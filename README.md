# recspecs

`recspecs` provides a lightweight expect testing facility for Racket. It is
inspired by [Jane Street's `expect_test` for OCaml](https://github.com/janestreet/ppx_expect)
and the [`expect-test` crate](https://github.com/rust-analyzer/expect-test) for Rust.

Expect tests are especially useful when the clearest behavior to check is
what a function prints. Put the code that produces output next to the output
you expect, run the file with `raco test`, and review the saved transcript
when behavior changes. Each `expect` form expands to a small RackUnit test.
When the environment variable `RECSPECS_UPDATE` is set, failing expectations
are automatically updated in the file instead of causing a failure.

For readable multi-line examples, use Racket's @-expression reader by
starting a test file with `#lang at-exp racket`. Then write `@expect[...]`:
the expression to run goes in square brackets, and the expected output goes
in braces.

## Getting started

```racket
#lang at-exp racket
(require recspecs)

(define (greet name)
  (printf "Hello, ~a!\n" name))

@expect[(greet "Ada")]{
Hello, Ada!
}
```

The `@expect[...] { ... }` shape is just reader syntax for an ordinary
`expect` call. It lets the expected output look like the text your program
prints, instead of a string with `\n` escapes.

A slightly larger example can still stay direct:

```racket
#lang at-exp racket
(require recspecs)

(define (print-shopping-list items)
  (for ([item items]
        [n (in-naturals 1)])
    (printf "~a. ~a\n" n item)))

@expect[(print-shopping-list '("apples" "bread" "coffee"))]{
1. apples
2. bread
3. coffee
}
```

Run the file normally:

```console
$ raco test shopping-list-test.rkt
```

When the output intentionally changes, update the recorded output and then
review the file:

```console
$ RECSPECS_UPDATE=1 raco test shopping-list-test.rkt
```

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
  `expect`, `expect-file`, or `capture-output`. Use `'both` to capture from
  both output ports at once.
* Use `capture-output` to run a thunk and return its printed output.

The Scribble reference shows accepted keyword arguments and their defaults in
each form signature.

## More common examples

Use `expect/print` when you want to check a returned value rather than
hand-writing a call to `display` or `printf`:

```racket
#lang at-exp racket
(require recspecs)

@expect/print[(map string-upcase '("red" "blue"))]{
'("RED" "BLUE")
}
```

Use `expect-exn` when the expected behavior is an exception message:

```racket
#lang at-exp racket
(require recspecs)

(define (parse-port n)
  (unless (and (integer? n) (<= 0 n 65535))
    (raise-user-error 'parse-port "expected an integer from 0 to 65535"))
  n)

@expect-exn[(parse-port 70000)]{
parse-port: expected an integer from 0 to 65535
}
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

