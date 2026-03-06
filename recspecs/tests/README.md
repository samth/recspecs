# Test Utilities

This directory contains auxiliary scripts used by the test suite.

## `bc.rkt`

A lightweight calculator implemented in Racket to mimic the basic
`bc` command. It supports addition, multiplication, division,
exponentiation using `^`, parentheses, and the `scale` variable for
setting decimal precision. The script reads expressions from standard
input and prints results, exiting when the input is `quit`.
