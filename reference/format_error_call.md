# Validate and format a function call for use in error messages

- `error_call()` takes either a frame environment or a call. If the
  input is an environment, `error_call()` acts like
  [`frame_call()`](https://rlang.r-lib.org/reference/stack.md) with some
  additional logic, e.g. for S3 methods and for frames with a
  [`local_error_call()`](https://rlang.r-lib.org/reference/local_error_call.md).

- `format_error_call()` simplifies its input to a simple call (see
  section below) and formats the result as code (using cli if
  available). Use this function to generate the "in" part of an error
  message from a stack frame call.

  `format_error_call()` first passes its input to `error_call()` to
  fetch calls from frame environments.

## Usage

``` r
format_error_call(call)

error_call(call)
```

## Arguments

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

## Value

Either a string formatted as code or `NULL` if a simple call could not
be generated.

## Details of formatting

- The arguments of function calls are stripped.

- Complex function calls containing inlined objects return `NULL`.

- Calls to `if` preserve the condition since it might be informative.
  Branches are dropped.

- Calls to operators and other special syntax are formatted using their
  names rather than the potentially confusing function form.

## Examples

``` r
# Arguments are stripped
writeLines(format_error_call(quote(foo(bar, baz))))
#> `foo()`

# Returns `NULL` with complex calls such as those that contain
# inlined functions
format_error_call(call2(list))
#> NULL

# Operators are formatted using their names rather than in
# function call form
writeLines(format_error_call(quote(1 + 2)))
#> `1 + 2`
```
