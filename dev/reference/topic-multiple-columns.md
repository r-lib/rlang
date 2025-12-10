# Taking multiple columns without `...`

In this guide we compare ways of taking multiple columns in a single
function argument.

As a refresher (see the [programming
patterns](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)
article), there are two common ways of passing arguments to
[data-masking](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
functions. For single arguments, embrace with
[`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md):

    my_group_by <- function(data, var) {
      data %>% dplyr::group_by({{ var }})
    }

    my_pivot_longer <- function(data, var) {
      data %>% tidyr::pivot_longer({{ var }})
    }

For multiple arguments in `...`, pass them on to functions that also
take `...` like `group_by()`, or pass them within
[`c()`](https://rdrr.io/r/base/c.html) for functions taking tidy
selection in a single argument like `pivot_longer()`:

    # Pass dots through
    my_group_by <- function(.data, ...) {
      .data %>% dplyr::group_by(...)
    }

    my_pivot_longer <- function(.data, ...) {
      .data %>% tidyr::pivot_longer(c(...))
    }

But what if you want to take multiple columns in a single named argument
rather than in `...`?

## Using tidy selections

The idiomatic tidyverse way of taking multiple columns in a single
argument is to take a *tidy selection* (see the [Argument
behaviours](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)
section). In tidy selections, the syntax for passing multiple columns in
a single argument is [`c()`](https://rdrr.io/r/base/c.html):

    mtcars %>% tidyr::pivot_longer(c(am, cyl, vs))

Since `{{` inherits behaviour, this implementation of
`my_pivot_longer()` automatically allows multiple columns passing:

    my_pivot_longer <- function(data, var) {
      data %>% tidyr::pivot_longer({{ var }})
    }

    mtcars %>% my_pivot_longer(c(am, cyl, vs))

For `group_by()`, which takes data-masked arguments, we'll use
`across()` as a *bridge* (see [Bridge
patterns](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)).

    my_group_by <- function(data, var) {
      data %>% dplyr::group_by(across({{ var }}))
    }

    mtcars %>% my_group_by(c(am, cyl, vs))

When embracing in tidyselect context or using `across()` is not
possible, you might have to implement tidyselect behaviour manually with
[`tidyselect::eval_select()`](https://tidyselect.r-lib.org/reference/eval_select.html).

## Using external defusal

To implement an argument with tidyselect behaviour, it is necessary to
[defuse](https://rlang.r-lib.org/dev/reference/topic-defuse.md) the
argument. However defusing an argument which had historically behaved
like a regular argument is a rather disruptive breaking change. This is
why we could not implement tidy selections in ggplot2 facetting
functions like `facet_grid()` and `facet_wrap()`.

An alternative is to use external defusal of arguments. This is what
formula interfaces do for instance. A modelling function takes a formula
in a regular argument and the formula defuses the user code:

    my_lm <- function(data, f, ...) {
      lm(f, data, ...)
    }

    mtcars %>% my_lm(disp ~ drat)

Once created, the defused expressions contained in the formula are
passed around like a normal argument. A similar approach was taken to
update `facet_` functions to tidy eval. The `vars()` function (a simple
alias to
[`quos()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md))
is provided so that users can defuse their arguments externally.

    ggplot2::facet_grid(
      ggplot2::vars(cyl),
      ggplot2::vars(am, vs)
    )

You can implement this approach by simply taking a list of defused
expressions as argument. This list can be passed the usual way to other
functions taking such lists:

    my_facet_grid <- function(rows, cols, ...) {
      ggplot2::facet_grid(rows, cols, ...)
    }

Or it can be spliced with
[`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md):

    my_group_by <- function(data, vars) {
      stopifnot(is_quosures(vars))
      data %>% dplyr::group_by(!!!vars)
    }

    mtcars %>% my_group_by(dplyr::vars(cyl, am))

## A non-approach: Parsing lists

Intuitively, many programmers who want to take a list of expressions in
a single argument try to defuse an argument and parse it. The user is
expected to supply multiple arguments within a
[`list()`](https://rdrr.io/r/base/list.html) expression. When such a
call is detected, the arguments are retrieved and spliced with `!!!`.
Otherwise, the user is assumed to have supplied a single argument which
is injected with `!!`. An implementation along these lines might look
like this:

    my_group_by <- function(data, vars) {
      vars <- enquo(vars)

      if (quo_is_call(vars, "list")) {
        expr <- quo_get_expr(vars)
        env <- quo_get_env(vars)
        args <- as_quosures(call_args(expr), env = env)
        data %>% dplyr::group_by(!!!args)
      } else {
        data %>% dplyr::group_by(!!vars)
      }
    }

This does work in simple cases:

    mtcars %>% my_group_by(cyl) %>% dplyr::group_vars()
    #> [1] "cyl"

    mtcars %>% my_group_by(list(cyl, am)) %>% dplyr::group_vars()
    #> [1] "cyl" "am"

However this parsing approach quickly shows limits:

    mtcars %>% my_group_by(list2(cyl, am))
    #> Error in `group_by()`: Can't add columns.
    #> i `..1 = list2(cyl, am)`.
    #> i `..1` must be size 32 or 1, not 2.

Also, it would be better for overall consistency of interfaces to use
the tidyselect syntax [`c()`](https://rdrr.io/r/base/c.html) for passing
multiple columns. In general, we recommend to use either the tidyselect
or the external defusal approaches.
