# Including function calls in error messages

Starting with rlang 1.0,
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) includes the
erroring function in the message by default:

    my_function <- function() {
      abort("Can't do that.")
    }

    my_function()
    #> Error in `my_function()`:
    #> ! Can't do that.

This works well when
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) is called
directly within the failing function. However, when the
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) call is
exported to another function (which we call an "error helper"), we need
to be explicit about which function
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) is throwing
an error for.

## Passing the user context

There are two main kinds of error helpers:

- Simple [`abort()`](https://rlang.r-lib.org/dev/reference/abort.md)
  wrappers. These often aim at adding classes and attributes to an error
  condition in a structured way:

      stop_my_class <- function(message) {
        abort(message, class = "my_class")
      }

- Input checking functions. An input checker is typically passed an
  input and an argument name. It throws an error if the input doesn't
  conform to expectations:

      check_string <- function(x, arg = "x") {
        if (!is_string(x)) {
          cli::cli_abort("{.arg {arg}} must be a string.")
        }
      }

In both cases, the default error call is not very helpful to the end
user because it reflects an internal function rather than a user
function:

    my_function <- function(x) {
      check_string(x)
      stop_my_class("Unimplemented")
    }

    my_function(NA)
    #> Error in `check_string()`:
    #> ! `x` must be a string.

    my_function("foo")
    #> Error in `stop_my_class()`:
    #> ! Unimplemented

To fix this, let
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) know about
the function that it is throwing the error for by passing the
corresponding function environment as the `call` argument:

    stop_my_class <- function(message, call = caller_env()) {
      abort(message, class = "my_class", call = call)
    }

    check_string <- function(x, arg = "x", call = caller_env()) {
      if (!is_string(x)) {
        cli::cli_abort("{.arg {arg}} must be a string.", call = call)
      }
    }

    my_function(NA)
    #> Error in `my_function()`:
    #> ! `x` must be a string.

    my_function("foo")
    #> Error in `my_function()`:
    #> ! Unimplemented

### Input checkers and [`caller_arg()`](https://rlang.r-lib.org/dev/reference/caller_arg.md)

The
[`caller_arg()`](https://rlang.r-lib.org/dev/reference/caller_arg.md)
helper is useful in input checkers which check an input on the behalf of
another function. Instead of hard-coding `arg = "x"`, and forcing the
callers to supply it if `"x"` is not the name of the argument being
checked, use
[`caller_arg()`](https://rlang.r-lib.org/dev/reference/caller_arg.md).

    check_string <- function(x,
                             arg = caller_arg(x),
                             call = caller_env()) {
      if (!is_string(x)) {
        cli::cli_abort("{.arg {arg}} must be a string.", call = call)
      }
    }

It is a combination of
[`substitute()`](https://rdrr.io/r/base/substitute.html) and
[`rlang::as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md)
which provides a more generally applicable default:

    my_function <- function(my_arg) {
      check_string(my_arg)
    }

    my_function(NA)
    #> Error in `my_function()`:
    #> ! `my_arg` must be a string.

## testthat workflow

Error snapshots are the main way of checking that the correct error call
is included in an error message. However you'll need to opt into a new
testthat display for warning and error snapshots. With the new display,
these are printed by rlang, including the `call` field. This makes it
easy to monitor the full appearance of warning and error messages as
they are displayed to users.

This display is not applied to all packages yet. With testthat 3.1.2,
depend explicitly on rlang \>= 1.0.0 to opt in. Starting from testthat
3.1.3, depending on rlang, no matter the version, is sufficient to opt
in. In the future, the new display will be enabled for all packages.

Once enabled, create error snapshots with:

    expect_snapshot(error = TRUE, {
      my_function()
    })

You'll have to make sure that the snapshot coverage for error messages
is sufficient for your package.
