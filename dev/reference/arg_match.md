# Match an argument to a character vector

This is equivalent to
[`base::match.arg()`](https://rdrr.io/r/base/match.arg.html) with a few
differences:

- Partial matches trigger an error.

- Error messages are a bit more informative and obey the tidyverse
  standards.

`arg_match()` derives the possible values from the [caller
function](https://rlang.r-lib.org/dev/reference/stack.md).

`arg_match0()` is a bare-bones version if performance is at a premium.
It requires a string as `arg` and explicit character `values`. For
convenience, `arg` may also be a character vector containing every
element of `values`, possibly permuted. In this case, the first element
of `arg` is used.

## Usage

``` r
arg_match(
  arg,
  values = NULL,
  ...,
  multiple = FALSE,
  error_arg = caller_arg(arg),
  error_call = caller_env()
)

arg_match0(arg, values, arg_nm = caller_arg(arg), error_call = caller_env())
```

## Arguments

- arg:

  A symbol referring to an argument accepting strings.

- values:

  A character vector of possible values that `arg` can take.

- ...:

  These dots are for future extensions and must be empty.

- multiple:

  Whether `arg` may contain zero or several values.

- error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/dev/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) for more
  information.

- arg_nm:

  Same as `error_arg`.

## Value

The string supplied to `arg`.

## See also

[`check_required()`](https://rlang.r-lib.org/dev/reference/check_required.md)

## Examples

``` r
fn <- function(x = c("foo", "bar")) arg_match(x)
fn("bar")
#> [1] "bar"

# Throws an informative error for mismatches:
try(fn("b"))
#> Error in fn("b") : `x` must be one of "foo" or "bar", not "b".
#> ℹ Did you mean "bar"?
try(fn("baz"))
#> Error in fn("baz") : `x` must be one of "foo" or "bar", not "baz".
#> ℹ Did you mean "bar"?

# Use the bare-bones version with explicit values for speed:
arg_match0("bar", c("foo", "bar", "baz"))
#> [1] "bar"

# For convenience:
fn1 <- function(x = c("bar", "baz", "foo")) fn3(x)
fn2 <- function(x = c("baz", "bar", "foo")) fn3(x)
fn3 <- function(x) arg_match0(x, c("foo", "bar", "baz"))
fn1()
#> [1] "bar"
fn2("bar")
#> [1] "bar"
try(fn3("zoo"))
#> Error in fn3("zoo") : 
#>   `x` must be one of "foo", "bar", or "baz", not "zoo".
#> ℹ Did you mean "foo"?
```
