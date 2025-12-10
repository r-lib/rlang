# Including contextual information with error chains

Error chaining is a mechanism for providing contextual information when
an error occurs. There are multiple situations in which you might be
able to provide context that is helpful to quickly understand the cause
or origin of an error:

- Mentioning the *high level context* in which a low level error arose.
  E.g. chaining a low-level HTTP error to a high-level download error.

- Mentioning the *pipeline step* in which a user error occurred. This is
  a major use-case for NSE interfaces in the tidyverse, e.g. in dplyr,
  tidymodels or ggplot2.

- Mentioning the *iteration context* in which a user error occurred. For
  instance, the input file when processing documents, or the iteration
  number or key when running user code in a loop.

Here is an example of a chained error from dplyr that shows the pipeline
step (`mutate()`) and the iteration context (group ID) in which a
function called by the user failed:

    add <- function(x, y) x + y

    mtcars |>
      dplyr::group_by(cyl) |>
      dplyr::mutate(new = add(disp, "foo"))
    #> Error in `dplyr::mutate()`:
    #> i In argument: `new = add(disp, "foo")`.
    #> i In group 1: `cyl = 4`.
    #> Caused by error in `x + y`:
    #> ! non-numeric argument to binary operator

In all these cases, there are two errors in play, chained together:

1.  The **causal error**, which interrupted the current course of
    action.

2.  The **contextual error**, which expresses higher-level information
    when something goes wrong.

There may be more than one contextual error in an error chain, but there
is always only one causal error.

## Rethrowing errors

To create an error chain, you must first capture causal errors when they
occur. We recommend using
[`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md)
instead of [`tryCatch()`](https://rdrr.io/r/base/conditions.html) or
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).

- Compared to [`tryCatch()`](https://rdrr.io/r/base/conditions.html),
  [`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md)
  fully preserves the context of the error. This is important for
  debugging because it ensures complete backtraces are reported to users
  (e.g. via
  [`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md))
  and allows `options(error = recover)` to reach into the deepest error
  context.

- Compared to
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html),
  which also preserves the error context,
  [`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md) is
  able to catch stack overflow errors on R versions \>= 4.2.0.

In practice,
[`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md)
works just like [`tryCatch()`](https://rdrr.io/r/base/conditions.html).
It takes pairs of error class names and handling functions. To chain an
error, simply rethrow it from an error handler by passing it as `parent`
argument.

In this example, we'll create a `with_` function. That is, a function
that sets up some configuration (in this case, chained errors) before
executing code supplied as input:

    with_chained_errors <- function(expr) {
      try_fetch(
        expr,
        error = function(cnd) {
          abort("Problem during step.", parent = cnd)
        }
      )
    }

    with_chained_errors(1 + "")
    #> Error in `with_chained_errors()`:
    #> ! Problem during step.
    #> Caused by error in `1 + ""`:
    #> ! non-numeric argument to binary operator

Typically, you'll use this error helper from another user-facing
function.

    my_verb <- function(expr) {
      with_chained_errors(expr)
    }

    my_verb(add(1, ""))
    #> Error in `with_chained_errors()`:
    #> ! Problem during step.
    #> Caused by error in `x + y`:
    #> ! non-numeric argument to binary operator

Altough we have created a chained error, the error call of the
contextual error is not quite right. It mentions the name of the error
helper instead of the name of the user-facing function.

If you've read [Including function calls in error
messages](https://rlang.r-lib.org/dev/reference/topic-error-call.md),
you may suspect that we need to pass a `call` argument to
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md). That's
exactly what needs to happen to fix the call and backtrace issues:

    with_chained_errors <- function(expr, call = caller_env()) {
      try_fetch(
        expr,
        error = function(cnd) {
          abort("Problem during step.", parent = cnd, call = call)
        }
      )
    }

Now that we've passed the caller environment as `call` argument,
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md)
automatically picks up the corresponding function call from the
execution frame:

    my_verb(add(1, ""))
    #> Error in `my_verb()`:
    #> ! Problem during step.
    #> Caused by error in `x + y`:
    #> ! non-numeric argument to binary operator

### Side note about missing arguments

`my_verb()` is implemented with a lazy evaluation pattern. The user
input kept unevaluated until the error chain context is set up. A
downside of this arrangement is that missing argument errors are
reported in the wrong context:

    my_verb()
    #> Error in `my_verb()`:
    #> ! Problem during step.
    #> Caused by error in `my_verb()`:
    #> ! argument "expr" is missing, with no default

To fix this, simply require these arguments before setting up the
chained error context, for instance with the
[`check_required()`](https://rlang.r-lib.org/dev/reference/check_required.md)
input checker exported from rlang:

    my_verb <- function(expr) {
      check_required(expr)
      with_chained_errors(expr)
    }

    my_verb()
    #> Error in `my_verb()`:
    #> ! `expr` is absent but must be supplied.

## Taking full ownership of a causal error

It is also possible to completely take ownership of a causal error and
rethrow it with a more user-friendly error message. In this case, the
original error is completely hidden from the end user. Opting for his
approach instead of chaining should be carefully considered because
hiding the causal error may deprive users from precious debugging
information.

- In general, hiding *user errors* (e.g. dplyr inputs) in this way is
  likely a bad idea.

- It may be appropriate to hide low-level errors, e.g. replacing HTTP
  errors by a high-level download error. Similarly, tidyverse packages
  like dplyr are replacing low-level vctrs errors with higher level
  errors of their own crafting.

- Hiding causal errors indiscriminately should likely be avoided because
  it may suppress information about unexpected errors. In general,
  rethrowing an unchained errors should only be done with specific error
  classes.

To rethrow an error without chaining it, and completely take over the
causal error from the user point of view, fetch it with
[`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md) and
throw a new error. The only difference with throwing a chained error is
that the `parent` argument is set to `NA`. You could also omit the
`parent` argument entirely, but passing `NA` lets
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) know it is
rethrowing an error from a handler and that it should hide the
corresponding error helpers in the backtrace.

    with_own_scalar_errors <- function(expr, call = caller_env()) {
      try_fetch(
        expr,
        vctrs_error_scalar_type = function(cnd) {
          abort(
            "Must supply a vector.",
            parent = NA,
            error = cnd,
            call = call
          )
        }
      )
    }

    my_verb <- function(expr) {
      check_required(expr)
      with_own_scalar_errors(
        vctrs::vec_assert(expr)
      )
    }

    my_verb(env())
    #> Error in `my_verb()`:
    #> ! Must supply a vector.

When a low-level error is overtaken, it is good practice to store it in
the high-level error object, so that it can be inspected for debugging
purposes. In the snippet above, we stored it in the `error` field. Here
is one way of accessing the original error by subsetting the object
returned by
[`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md):

    rlang::last_error()$error

## Case study: Mapping with chained errors

One good use case for chained errors is adding information about the
iteration state when looping over a set of inputs. To illustrate this,
we'll implement a version of `map()` /
[`lapply()`](https://rdrr.io/r/base/lapply.html) that chains an
iteration error to any captured user error.

Here is a minimal implementation of `map()`:

    my_map <- function(.xs, .fn, ...) {
      out <- new_list(length(.xs))

      for (i in seq_along(.xs)) {
        out[[i]] <- .fn(.xs[[i]], ...)
      }

      out
    }

    list(1, 2) |> my_map(add, 100)
    #> [[1]]
    #> [1] 101
    #>
    #> [[2]]
    #> [1] 102

With this implementation, the user has no idea which iteration failed
when an error occurs:

    list(1, "foo") |> my_map(add, 100)
    #> Error in `x + y`:
    #> ! non-numeric argument to binary operator

### Rethrowing with iteration information

To improve on this we'll wrap the loop in a
[`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md) call
that rethrow errors with iteration information. Make sure to call
[`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md) on
the outside of the loop to avoid a massive performance hit:

    my_map <- function(.xs, .fn, ...) {
      out <- new_list(length(.xs))
      i <- 0L

      try_fetch(
        for (i in seq_along(.xs)) {
          out[[i]] <- .fn(.xs[[i]], ...)
        },
        error = function(cnd) {
          abort(
            sprintf("Problem while mapping element %d.", i),
            parent = cnd
          )
        }
      )

      out
    }

And that's it, the error chain created by the rethrowing handler now
provides users with the number of the failing iteration:

    list(1, "foo") |> my_map(add, 100)
    #> Error in `my_map()`:
    #> ! Problem while mapping element 2.
    #> Caused by error in `x + y`:
    #> ! non-numeric argument to binary operator

### Dealing with errors thrown from the mapped function

One problem though, is that the user error call is not very informative
when the error occurs immediately in the function supplied to
`my_map()`:

    my_function <- function(x) {
      if (!is_string(x)) {
        abort("`x` must be a string.")
      }
    }

    list(1, "foo") |> my_map(my_function)
    #> Error in `my_map()`:
    #> ! Problem while mapping element 1.
    #> Caused by error in `.fn()`:
    #> ! `x` must be a string.

Functions have no names by themselves. Only the variable that refers to
the function has a name. In this case, the mapped function is passed by
argument to the variable `.fn`. So, when an error happens, this is the
name that is reported to users.

One approach to fix this is to inspect the `call` field of the error.
When we detect a `.fn` call, we replace it by the defused code supplied
as `.fn` argument:

    my_map <- function(.xs, .fn, ...) {
      # Capture the defused code supplied as `.fn`
      fn_code <- substitute(.fn)

      out <- new_list(length(.xs))

      for (i in seq_along(.xs)) {
        try_fetch(
          out[[i]] <- .fn(.xs[[i]], ...),
          error = function(cnd) {
            # Inspect the `call` field to detect `.fn` calls
            if (is_call(cnd$call, ".fn")) {
              # Replace `.fn` by the defused code.
              # Keep existing arguments.
              cnd$call[[1]] <- fn_code
            }
            abort(
              sprintf("Problem while mapping element %s.", i),
              parent = cnd
            )
          }
        )
      }

      out
    }

And voilà!

    list(1, "foo") |> my_map(my_function)
    #> Error in `my_map()`:
    #> ! Problem while mapping element 1.
    #> Caused by error in `my_function()`:
    #> ! `x` must be a string.
