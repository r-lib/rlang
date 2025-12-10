# Why are strings and other constants enquosed in the empty environment?

Function arguments are
[defused](https://rlang.r-lib.org/reference/topic-defuse.md) into
[quosures](https://rlang.r-lib.org/reference/topic-quosure.md) that keep
track of the environment of the defused expression.

    quo(1 + 1)
    #> <quosure>
    #> expr: ^1 + 1
    #> env:  global

You might have noticed that when constants are supplied, the quosure
tracks the empty environment instead of the current environmnent.

    quos("foo", 1, NULL)
    #> <list_of<quosure>>
    #>
    #> [[1]]
    #> <quosure>
    #> expr: ^"foo"
    #> env:  empty
    #>
    #> [[2]]
    #> <quosure>
    #> expr: ^1
    #> env:  empty
    #>
    #> [[3]]
    #> <quosure>
    #> expr: ^NULL
    #> env:  empty

The reason for this has to do with compilation of R code which makes it
impossible to consistently capture environments of constants from
function arguments. Argument defusing relies on the *promise* mechanism
of R for lazy evaluation of arguments. When functions are compiled and R
notices that an argument is constant, it avoids creating a promise since
they slow down function evaluation. Instead, the function is directly
supplied a naked constant instead of constant wrapped in a promise.

## Concrete case of promise unwrapping by compilation

We can observe this optimisation by calling into the C-level `findVar()`
function to capture promises.

    # Return the object bound to `arg` without triggering evaluation of
    # promises
    f <- function(arg) {
      rlang:::find_var(current_env(), sym("arg"))
    }

    # Call `f()` with a symbol or with a constant
    g <- function(symbolic) {
      if (symbolic) {
        f(letters)
      } else {
        f("foo")
      }
    }

    # Make sure these small functions are compiled
    f <- compiler::cmpfun(f)
    g <- compiler::cmpfun(g)

When `f()` is called with a symbolic argument, we get the promise object
created by R.

    g(symbolic = TRUE)
    #> <promise: 0x7ffd79bac130>

However, supplying a constant to `"f"` returns the constant directly.

    g(symbolic = FALSE)
    #> [1] "foo"

Without a promise, there is no way to figure out the original
environment of an argument.

## Do we need environments for constants?

Data-masking APIs in the tidyverse are intentionally designed so that
they don't need an environment for constants.

- Data-masking APIs should be able to interpret constants. These can
  arise from normal argument passing as we have seen, or by
  [injection](https://rlang.r-lib.org/reference/topic-inject.md) with
  `!!`. There should be no difference between
  `dplyr::mutate(mtcars, var = cyl)` and
  `dplyr::mutate(mtcars, var = !!mtcars$cyl)`.

- Data-masking is an *evaluation* idiom, not an *introspective* one. The
  behaviour of data-masking function should not depend on the calling
  environment when a constant (or a symbol evaluating to a given value)
  is supplied.
