# Temporarily change bindings of an environment

- `local_bindings()` temporarily changes bindings in `.env` (which is by
  default the caller environment). The bindings are reset to their
  original values when the current frame (or an arbitrary one if you
  specify `.frame`) goes out of scope.

- `with_bindings()` evaluates `expr` with temporary bindings. When
  `with_bindings()` returns, bindings are reset to their original
  values. It is a simple wrapper around `local_bindings()`.

## Usage

``` r
local_bindings(..., .env = .frame, .frame = caller_env())

with_bindings(.expr, ..., .env = caller_env())
```

## Arguments

- ...:

  Pairs of names and values. These dots support splicing (with value
  semantics) and name unquoting.

- .env:

  An environment.

- .frame:

  The frame environment that determines the scope of the temporary
  bindings. When that frame is popped from the call stack, bindings are
  switched back to their original values.

- .expr:

  An expression to evaluate with temporary bindings.

## Value

`local_bindings()` returns the values of old bindings invisibly;
`with_bindings()` returns the value of `expr`.

## Examples

``` r
foo <- "foo"
bar <- "bar"

# `foo` will be temporarily rebinded while executing `expr`
with_bindings(paste(foo, bar), foo = "rebinded")
#> [1] "rebinded bar"
paste(foo, bar)
#> [1] "foo bar"
```
