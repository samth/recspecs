# recspecs

`recspecs` provides a lightweight expect testing facility for Racket. It is
inspired by [Jane Street's `expect_test` for OCaml](https://github.com/janestreet/expect_test)
and the [`expect-test` crate](https://github.com/greyblake/expect-test) for Rust.

Expect tests record the output of expressions directly in the source file.
Each `expect` form expands to a small RackUnit test that compares the
captured output against the recorded expectation. When the environment
variable `RECSPECS_UPDATE` is set, failing expectations are automatically
updated in the file instead of causing a failure.

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

Run the file with `raco test` (or any RackUnit runner) to execute the
expectations. If they fail and you want to update the saved output, set
`RECSPECS_UPDATE`:

```console
$ RECSPECS_UPDATE=1 raco test my-test.rkt
```

## Status

This library is experimental but demonstrates the core API.  More features,
like integration with rackunit and pretty diff output, can be added in the
future.

