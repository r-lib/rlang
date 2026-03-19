# Check argument type (numbers)

**\[experimental\]**

These functions check that an argument is a number, optionally with
bounds, and produce friendly error messages otherwise.

## Usage

``` r
check_number_decimal(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_number_whole(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = FALSE,
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
  [`abort()`](https://rlang.r-lib.org/dev/reference/abort.md).

- min:

  Minimum value (inclusive). If `NULL`, no lower bound is checked.

- max:

  Maximum value (inclusive). If `NULL`, no upper bound is checked.

- allow_infinite:

  Whether infinite values are allowed.

- allow_na:

  Whether `NA` values are allowed.

- allow_null:

  Whether `NULL` is allowed.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/dev/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) for more
  information.

## Value

`NULL` invisibly if the check passes, throws an error otherwise.

## See also

Other input checkers:
[`check_data_frame()`](https://rlang.r-lib.org/dev/reference/check_data_frame.md),
[`check_type_scalar`](https://rlang.r-lib.org/dev/reference/check_type_scalar.md),
[`stop_input_type()`](https://rlang.r-lib.org/dev/reference/stop_input_type.md)

## Examples

``` r
check_number_decimal(3.14)
try(check_number_decimal("x"))
#> Error in eval(expr, envir) : 
#>   `"x"` must be a number, not the string "x".

check_number_whole(42)
try(check_number_whole(3.5))
#> Error in eval(expr, envir) : 
#>   `3.5` must be a whole number, not the number 3.5.
```
