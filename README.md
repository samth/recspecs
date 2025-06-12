# recspecs

`recspecs` provides a lightweight expect testing facility for Racket. It is
inspired by [Jane Street's `expect_test` for OCaml](https://github.com/janestreet/expect_test)
and the [`expect-test` crate](https://github.com/greyblake/expect-test) for Rust.

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
* All expectation forms accept multiple string arguments which are
  concatenated together. This is handy when using
  `#lang at-exp` for multi-line expectations.
* Setting the `RECSPECS_UPDATE_TEST` environment variable to a test case
  name limits updates to only that expectation.
* `capture-output` runs a thunk and returns everything it prints.

## Example

```racket
#lang racket
(require recspecs)

(expect
  (begin
    (displayln "hello")
    (displayln (+ 1 2)))
  "hello\n3\n")
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

The helper `capture-output` runs a thunk and returns everything it prints:

```racket
(capture-output (lambda () (display "hi")))
;; => "hi"
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

### Emacs integration

The file `emacs/recspecs.el` defines a helper command
`recspecs-update-at-point`.  When called from a buffer visiting a Racket
file under `racket-mode`, it reruns that file with
`RECSPECS_UPDATE` enabled and sets `RECSPECS_UPDATE_TEST` to the
expectation at point so only that one is updated.

## Status

This library is experimental but demonstrates the core API. It now shows a
colorized diff when expectations fail and supports file based expectations
and exception checks. Further features like tighter integration with
rackunit can be added in the future.

