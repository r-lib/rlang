# Check argument type (scalar)

**\[experimental\]**

These functions check that an argument is of the expected scalar type
and produce friendly error messages otherwise.

## Usage

``` r
check_bool(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_string(
  x,
  ...,
  allow_empty = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  The argument to check.

- ...:

  Additional arguments passed to
  [`abort()`](https://rlang.r-lib.org/reference/abort.md).

- allow_na:

  Whether `NA` values are allowed.

- allow_null:

  Whether `NULL` is allowed.

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

- allow_empty:

  Whether the empty string `""` is allowed (`check_string()` only).

## Value

`NULL` invisibly if the check passes, throws an error otherwise.

## See also

Other input checkers:
[`check_data_frame()`](https://rlang.r-lib.org/reference/check_data_frame.md),
[`check_type_number`](https://rlang.r-lib.org/reference/check_type_number.md),
[`stop_input_type()`](https://rlang.r-lib.org/reference/stop_input_type.md)

## Examples

``` r
check_bool(TRUE)
try(check_bool(1))
#> Error in eval(expr, envir) : 
#>   `1` must be `TRUE` or `FALSE`, not the number 1.

check_string("hello")
try(check_string(42))
#> Error in eval(expr, envir) : 
#>   `42` must be a single string, not the number 42.
```
