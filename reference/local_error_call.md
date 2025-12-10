# Set local error call in an execution environment

`local_error_call()` is an alternative to explicitly passing a `call`
argument to [`abort()`](https://rlang.r-lib.org/reference/abort.md). It
sets the call (or a value that indicates where to find the call, see
below) in a local binding that is automatically picked up by
[`abort()`](https://rlang.r-lib.org/reference/abort.md).

## Usage

``` r
local_error_call(call, frame = caller_env())
```

## Arguments

- call:

  This can be:

  - A call to be used as context for an error thrown in that execution
    environment.

  - The `NULL` value to show no context.

  - An execution environment, e.g. as returned by
    [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
    [`sys.call()`](https://rdrr.io/r/base/sys.parent.html) for that
    environment is taken as context.

- frame:

  The execution environment in which to set the local error call.

## Motivation for setting local error calls

By default [`abort()`](https://rlang.r-lib.org/reference/abort.md) uses
the function call of its caller as context in error messages:

    foo <- function() abort("Uh oh.")
    foo()
    #> Error in `foo()`: Uh oh.

This is not always appropriate. For example a function that checks an
input on the behalf of another function should reference the latter, not
the former:

    arg_check <- function(arg,
                          error_arg = as_string(substitute(arg))) {
      abort(cli::format_error("{.arg {error_arg}} is failing."))
    }

    foo <- function(x) arg_check(x)
    foo()
    #> Error in `arg_check()`: `x` is failing.

The mismatch is clear in the example above. `arg_check()` does not have
any `x` argument and so it is confusing to present `arg_check()` as
being the relevant context for the failure of the `x` argument.

One way around this is to take a `call` or `error_call` argument and
pass it to [`abort()`](https://rlang.r-lib.org/reference/abort.md). Here
we name this argument `error_call` for consistency with `error_arg`
which is prefixed because there is an existing `arg` argument. In other
situations, taking `arg` and `call` arguments might be appropriate.

    arg_check <- function(arg,
                          error_arg = as_string(substitute(arg)),
                          error_call = caller_env()) {
      abort(
        cli::format_error("{.arg {error_arg}} is failing."),
        call = error_call
      )
    }

    foo <- function(x) arg_check(x)
    foo()
    #> Error in `foo()`: `x` is failing.

This is the generally recommended pattern for argument checking
functions. If you mention an argument in an error message, provide your
callers a way to supply a different argument name and a different error
call. [`abort()`](https://rlang.r-lib.org/reference/abort.md) stores the
error call in the `call` condition field which is then used to generate
the "in" part of error messages.

In more complex cases it's often burdensome to pass the relevant call
around, for instance if your checking and throwing code is structured
into many different functions. In this case, use `local_error_call()` to
set the call locally or instruct
[`abort()`](https://rlang.r-lib.org/reference/abort.md) to climb the
call stack one level to find the relevant call. In the following
example, the complexity is not so important that sparing the argument
passing makes a big difference. However this illustrates the pattern:

    arg_check <- function(arg,
                          error_arg = caller_arg(arg),
                          error_call = caller_env()) {
      # Set the local error call
      local_error_call(error_call)

      my_classed_stop(
        cli::format_error("{.arg {error_arg}} is failing.")
      )
    }

    my_classed_stop <- function(message) {
      # Forward the local error call to the caller's
      local_error_call(caller_env())

      abort(message, class = "my_class")
    }

    foo <- function(x) arg_check(x)
    foo()
    #> Error in `foo()`: `x` is failing.

## Error call flags in performance-critical functions

The `call` argument can also be the string `"caller"`. This is
equivalent to
[`caller_env()`](https://rlang.r-lib.org/reference/stack.md) or
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) but has a
lower overhead because call stack introspection is only performed when
an error is triggered. Note that eagerly calling
[`caller_env()`](https://rlang.r-lib.org/reference/stack.md) is fast
enough in almost all cases.

If your function needs to be really fast, assign the error call flag
directly instead of calling `local_error_call()`:

    .__error_call__. <- "caller"

## Examples

``` r
# Set a context for error messages
function() {
  local_error_call(quote(foo()))
  local_error_call(sys.call())
}
#> function () 
#> {
#>     local_error_call(quote(foo()))
#>     local_error_call(sys.call())
#> }
#> <environment: 0x560f93a596b0>

# Disable the context
function() {
  local_error_call(NULL)
}
#> function () 
#> {
#>     local_error_call(NULL)
#> }
#> <environment: 0x560f93a596b0>

# Use the caller's context
function() {
  local_error_call(caller_env())
}
#> function () 
#> {
#>     local_error_call(caller_env())
#> }
#> <environment: 0x560f93a596b0>
```
