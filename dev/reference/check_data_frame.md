# Check argument type (data frame)

**\[experimental\]**

Checks that an argument is a data frame, producing a friendly error
message on failure.

## Usage

``` r
check_data_frame(
  x,
  ...,
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
[`check_type_number`](https://rlang.r-lib.org/dev/reference/check_type_number.md),
[`check_type_scalar`](https://rlang.r-lib.org/dev/reference/check_type_scalar.md),
[`stop_input_type()`](https://rlang.r-lib.org/dev/reference/stop_input_type.md)

## Examples

``` r
check_data_frame(mtcars)

try(check_data_frame(1:5))
#> Error in eval(expr, envir) : 
#>   `1:5` must be a data frame, not an integer vector.
```
