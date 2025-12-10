# What are quosures and when are they needed?

A quosure is a special type of [defused
expression](https://rlang.r-lib.org/dev/reference/topic-defuse.md) that
keeps track of the original context the expression was written in. The
tracking capabilities of quosures is important when interfacing
[data-masking](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
functions together because the functions might come from two unrelated
environments, like two different packages.

## Blending environments

Let's take an example where the R user calls the function
`summarise_bmi()` from the foo package to summarise a data frame with
statistics of a BMI value. Because the `height` variable of their data
frame is not in metres, they use a custom function `div100()` to rescale
the column.

    # Global environment of user

    div100 <- function(x) {
      x / 100
    }

    dplyr::starwars %>%
      foo::summarise_bmi(mass, div100(height))

The `summarise_bmi()` function is a data-masking function defined in the
namespace of the foo package which looks like this:

    # Namespace of package foo

    bmi <- function(mass, height) {
      mass / height^2
    }

    summarise_bmi <- function(data, mass, height) {
      data %>%
        bar::summarise_stats(bmi({{ mass }}, {{ height }}))
    }

The foo package uses the custom function `bmi()` to perform a
computation on two vectors. It interfaces with `summarise_stats()`
defined in bar, another package whose namespace looks like this:

    # Namespace of package bar

    check_numeric <- function(x) {
      stopifnot(is.numeric(x))
      x
    }

    summarise_stats <- function(data, var) {
      data %>%
        dplyr::transmute(
          var = check_numeric({{ var }})
        ) %>%
        dplyr::summarise(
          mean = mean(var, na.rm = TRUE),
          sd = sd(var, na.rm = TRUE)
        )
    }

Again the package bar uses a custom function, `check_numeric()`, to
validate its input. It also interfaces with data-masking functions from
dplyr (using the
[define-a-constant](https://rlang.r-lib.org/dev/reference/topic-double-evaluation.md)
trick to avoid issues of double evaluation).

There are three data-masking functions simultaneously interfacing in
this snippet:

- At the bottom,
  [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  takes a data-masked input, and creates a data frame of a single column
  named `var`.

- Before this, `bar::summarise_stats()` takes a data-masked input inside
  [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  and checks it is numeric.

- And first of all, `foo::summarise_bmi()` takes two data-masked inputs
  inside `bar::summarise_stats()` and transforms them to a single BMI
  value.

There is a fourth context, the global environment where
`summarise_bmi()` is called with two columns defined in a data frame,
one of which is transformed on the fly with the user function
`div100()`.

All of these contexts (except to some extent the global environment)
contain functions that are private and invisible to foreign functions.
Yet, the final expanded data-masked expression that is evaluated down
the line looks like this (with caret characters indicating the quosure
boundaries):

    dplyr::transmute(
      var = ^check_numeric(^bmi(^mass, ^div100(height)))
    )

The role of quosures is to let R know that `check_numeric()` should be
found in the bar package, `bmi()` in the foo package, and `div100()` in
the global environment.

## When should I create quosures?

As a tidyverse user you generally don't need to worry about quosures
because `{{` and `...` will create them for you. Introductory texts like
[Programming with
dplyr](https://dplyr.tidyverse.org/articles/programming.html) or the
[standard data-mask programming
patterns](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)
don't even mention the term. In more complex cases you might need to
create quosures with
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) or
[`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md) (even
though you generally don't need to know or care that these functions
return quosures). In this section, we explore when quosures are
necessary in these more advanced applications.

### Foreign and local expressions

As a rule of thumb, quosures are only needed for arguments defused with
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) or
[`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md) (or with
[`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md) which
calls [`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md)
implicitly):

    my_function <- function(var) {
      var <- enquo(var)
      their_function(!!var)
    }

    # Equivalently
    my_function <- function(var) {
      their_function({{ var }})
    }

Wrapping defused arguments in quosures is needed because expressions
supplied as argument comes from a different environment, the environment
of your user. For local expressions created in your function, you
generally don't need to create quosures:

    my_mean <- function(data, var) {
      # `expr()` is sufficient, no need for `quo()`
      expr <- expr(mean({{ var }}))
      dplyr::summarise(data, !!expr)
    }

    my_mean(mtcars, cyl)
    #> # A tibble: 1 x 1
    #>   `mean(cyl)`
    #>         <dbl>
    #> 1        6.19

Using
[`quo()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)
instead of [`expr()`](https://rlang.r-lib.org/dev/reference/expr.md)
would have worked too but it is superfluous because
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
which uses [`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md),
is already in charge of wrapping your expression within a quosure scoped
in your environment.

The same applies if you evaluate manually. By default,
[`eval()`](https://rdrr.io/r/base/eval.html) and
[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md)
capture your environment:

    my_mean <- function(data, var) {
      expr <- expr(mean({{ var }}))
      eval_tidy(expr, data)
    }

    my_mean(mtcars, cyl)
    #> [1] 6.1875

### External defusing

An exception to this rule of thumb (wrap foreign expressions in
quosures, not your own expressions) arises when your function takes
multiple expressions in a list instead of `...`. The preferred approach
in that case is to take a tidy selection so that users can combine
multiple columns using [`c()`](https://rdrr.io/r/base/c.html). If that
is not possible, you can take a list of externally defused expressions:

    my_group_by <- function(data, vars) {
      stopifnot(is_quosures(vars))
      data %>% dplyr::group_by(!!!vars)
    }

    mtcars %>% my_group_by(dplyr::vars(cyl, am))

In this pattern,
[`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html)
defuses expressions externally. It creates a list of quosures because
the expressions are passed around from function to function like regular
arguments. In fact,
[`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html) and
`ggplot2::vars()` are simple aliases of
[`quos()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md).

    dplyr::vars(cyl, am)
    #> <list_of<quosure>>
    #>
    #> [[1]]
    #> <quosure>
    #> expr: ^cyl
    #> env:  global
    #>
    #> [[2]]
    #> <quosure>
    #> expr: ^am
    #> env:  global

For more information about external defusing, see [Taking multiple
columns without
...](https://rlang.r-lib.org/dev/reference/topic-multiple-columns.md).

## Technical description of quosures

A quosure carries two things:

- An expression (get it with
  [`quo_get_expr()`](https://rlang.r-lib.org/dev/reference/quosure-tools.md)).

- An environment (get it with
  [`quo_get_env()`](https://rlang.r-lib.org/dev/reference/quosure-tools.md)).

And implements these behaviours:

- It is *callable*. Evaluation produces a result.

  For historical reasons,
  [`base::eval()`](https://rdrr.io/r/base/eval.html) doesn't support
  quosure evaluation. Quosures currently require
  [`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md).
  We would like to fix this limitation in the future.

- It is *hygienic*. It evaluates in the tracked environment.

- It is *maskable*. If evaluated in a data mask (currently only masks
  created with
  [`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) or
  [`new_data_mask()`](https://rlang.r-lib.org/dev/reference/as_data_mask.md)),
  the mask comes first in scope before the quosure environment.

  Conceptually, a quosure inherits from two chains of environments, the
  data mask and the user environment. In practice rlang implements this
  special scoping by rechaining the top of the data mask to the quosure
  environment currently under evaluation.

There are similarities between promises (the ones R uses to implement
lazy evaluation, not the async expressions from the promises package)
and quosures. One important difference is that promises are only
evaluated once and cache the result for subsequent evaluation. Quosures
behave more like calls and can be evaluated repeatedly, potentially in a
different data mask. This property is useful to implement
split-apply-combine evaluations.

## See also

- [`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) and
  [`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md) to defuse
  function arguments as quosures. This is the main way quosures are
  created.

- [`quo()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)
  which is like
  [`expr()`](https://rlang.r-lib.org/dev/reference/expr.md) but wraps in
  a quosure. Usually it is not needed to wrap local expressions
  yourself.

- [`quo_get_expr()`](https://rlang.r-lib.org/dev/reference/quosure-tools.md)
  and
  [`quo_get_env()`](https://rlang.r-lib.org/dev/reference/quosure-tools.md)
  to access quosure components.

- [`new_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md)
  and
  [`as_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md)
  to assemble a quosure from components.
