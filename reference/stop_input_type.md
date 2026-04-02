# Throw a type mismatch error

**\[experimental\]**

`stop_input_type()` throws an error when an argument has the wrong type,
producing a friendly error message that includes the expected type and
the actual type of the input.

## Usage

``` r
stop_input_type(
  x,
  what,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  show_value = TRUE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  The object that does not conform to `what`. Its "friendly type" is
  used in the error message.

- what:

  The friendly expected type as a string. Can be a character vector of
  expected types, in which case the error message mentions all of them
  in an "or" enumeration.

- ...:

  Additional arguments passed to
  [`abort()`](https://rlang.r-lib.org/reference/abort.md).

- allow_na:

  If `TRUE`, an `NA` description is appended to `what` in the error
  message.

- allow_null:

  If `TRUE`, a `NULL` description is appended to `what` in the error
  message.

- show_value:

  Whether to show the actual value in the error message.

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

## Value

Throws an error, does not return.

## See also

Other input checkers:
[`check_data_frame()`](https://rlang.r-lib.org/reference/check_data_frame.md),
[`check_type_number`](https://rlang.r-lib.org/reference/check_type_number.md),
[`check_type_scalar`](https://rlang.r-lib.org/reference/check_type_scalar.md)

## Examples

``` r
check_character <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_character(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a character vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# Succeeds
check_character(letters)

# Fails
try(check_character(42))
#> Error in eval(expr, envir) : 
#>   `42` must be a character vector, not the number 42.
```
