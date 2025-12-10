# Check that argument is supplied

Throws an error if `x` is missing.

## Usage

``` r
check_required(x, arg = caller_arg(x), call = caller_env())
```

## Arguments

- x:

  A function argument. Must be a symbol.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

## See also

[`arg_match()`](https://rlang.r-lib.org/reference/arg_match.md)

## Examples

``` r
f <- function(x)  {
  check_required(x)
}

# Fails because `x` is not supplied
try(f())
#> Error in f() : `x` is absent but must be supplied.

# Succeeds
f(NULL)
```
