# Standardise a call

**\[deprecated\]**

Deprecated in rlang 0.4.11 in favour of
[`call_match()`](https://rlang.r-lib.org/dev/reference/call_match.md).
`call_standardise()` was designed for call wrappers that include an
environment like formulas or quosures. The function definition was
plucked from that environment. However in practice it is rare to use it
with wrapped calls, and then it's easy to forget to supply the
environment. For these reasons, we have designed
[`call_match()`](https://rlang.r-lib.org/dev/reference/call_match.md) as
a simpler wrapper around
[`match.call()`](https://rdrr.io/r/base/match.call.html).

This is essentially equivalent to
[`base::match.call()`](https://rdrr.io/r/base/match.call.html), but with
experimental handling of primitive functions.

## Usage

``` r
call_standardise(call, env = caller_env())
```

## Arguments

- call, env:

  **\[deprecated\]**

## Value

A quosure if `call` is a quosure, a raw call otherwise.
