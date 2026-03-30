# Splice operator `!!!`

The splice operator `!!!` implemented in [dynamic
dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md) injects a list
of arguments into a function call. It belongs to the family of
[injection](https://rlang.r-lib.org/dev/reference/topic-inject.md)
operators and provides the same functionality as
[`do.call()`](https://rdrr.io/r/base/do.call.html).

The two main cases for splice injection are:

- Turning a list of inputs into distinct arguments. This is especially
  useful with functions that take data in `...`, such as
  [`base::rbind()`](https://rdrr.io/r/base/cbind.html).

      dfs <- list(mtcars, mtcars)
      inject(rbind(!!!dfs))

- Injecting [defused
  expressions](https://rlang.r-lib.org/dev/reference/topic-defuse.md)
  like [symbolised](https://rlang.r-lib.org/dev/reference/sym.md) column
  names.

  For tidyverse APIs, this second case is no longer as useful since
  dplyr 1.0 and the `across()` operator.

## Where does `!!!` work?

`!!!` does not work everywhere, you can only use it within certain
special functions:

- Functions taking [dynamic
  dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md) like
  [`list2()`](https://rlang.r-lib.org/dev/reference/list2.md).

- Functions taking
  [defused](https://rlang.r-lib.org/dev/reference/topic-defuse.md) and
  [data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
  arguments, which are dynamic by default.

- Inside [`inject()`](https://rlang.r-lib.org/dev/reference/inject.md).

Most tidyverse functions support `!!!` out of the box. With base
functions you need to use
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md) to enable
`!!!`.

Using the operator out of context may lead to incorrect results, see
[What happens if I use injection operators out of
context?](https://rlang.r-lib.org/dev/reference/topic-inject-out-of-context.md).

## Splicing a list of arguments

Take a function like
[`base::rbind()`](https://rdrr.io/r/base/cbind.html) that takes data in
`...`. This sort of functions takes a variable number of arguments.

    df1 <- data.frame(x = 1)
    df2 <- data.frame(x = 2)

    rbind(df1, df2)
    #>   x
    #> 1 1
    #> 2 2

Passing individual arguments is only possible for a fixed amount of
arguments. When the arguments are in a list whose length is variable
(and potentially very large), we need a programmatic approach like the
splicing syntax `!!!`:

    dfs <- list(df1, df2)

    inject(rbind(!!!dfs))
    #>   x
    #> 1 1
    #> 2 2

Because [`rbind()`](https://rdrr.io/r/base/cbind.html) is a base
function we used
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md) to
explicitly enable `!!!`. However, many functions implement [dynamic
dots](https://rlang.r-lib.org/dev/reference/list2.md) with `!!!`
implicitly enabled out of the box.

    tidyr::expand_grid(x = 1:2, y = c("a", "b"))
    #> # A tibble: 4 x 2
    #>       x y
    #>   <int> <chr>
    #> 1     1 a
    #> 2     1 b
    #> 3     2 a
    #> 4     2 b

    xs <- list(x = 1:2, y = c("a", "b"))
    tidyr::expand_grid(!!!xs)
    #> # A tibble: 4 x 2
    #>       x y
    #>   <int> <chr>
    #> 1     1 a
    #> 2     1 b
    #> 3     2 a
    #> 4     2 b

Note how the expanded grid has the right column names. That's because we
spliced a *named* list. Splicing causes each name of the list to become
an argument name.

    tidyr::expand_grid(!!!set_names(xs, toupper))
    #> # A tibble: 4 x 2
    #>       X Y
    #>   <int> <chr>
    #> 1     1 a
    #> 2     1 b
    #> 3     2 a
    #> 4     2 b

## Splicing a list of expressions

Another usage for `!!!` is to inject [defused
expressions](https://rlang.r-lib.org/dev/reference/topic-defuse.md) into
[data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
dots. However this usage is no longer a common pattern for programming
with tidyverse functions and we recommend using other patterns if
possible.

First, instead of using the [defuse-and-inject
pattern](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)
with `...`, you can simply pass them on as you normally would. These two
expressions are completely equivalent:

    my_group_by <- function(.data, ...) {
      .data %>% dplyr::group_by(!!!enquos(...))
    }

    # This equivalent syntax is preferred
    my_group_by <- function(.data, ...) {
      .data %>% dplyr::group_by(...)
    }

Second, more complex applications such as [transformation
patterns](https://rlang.r-lib.org/dev/reference/topic-metaprogramming.md)
can be solved with the `across()` operation introduced in dplyr 1.0. Say
you want to take the [`mean()`](https://rdrr.io/r/base/mean.html) of all
expressions in `...`. Before `across()`, you had to defuse the `...`
expressions, wrap them in a call to
[`mean()`](https://rdrr.io/r/base/mean.html), and inject them in
`summarise()`.

    my_mean <- function(.data, ...) {
      # Defuse dots and auto-name them
      exprs <- enquos(..., .named = TRUE)

      # Wrap the expressions in a call to `mean()`
      exprs <- purrr::map(exprs, ~ call("mean", .x, na.rm = TRUE))

      # Inject them
      .data %>% dplyr::summarise(!!!exprs)
    }

It is much easier to use `across()` instead:

    my_mean <- function(.data, ...) {
      .data %>% dplyr::summarise(across(c(...), ~ mean(.x, na.rm = TRUE)))
    }

## Performance of injected dots and dynamic dots

Take this [dynamic
dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md) function:

    n_args <- function(...) {
      length(list2(...))
    }

Because it takes dynamic dots you can splice with `!!!` out of the box.

    n_args(1, 2)
    #> [1] 2

    n_args(!!!mtcars)
    #> [1] 11

Equivalently you could enable `!!!` explicitly with
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md).

    inject(n_args(!!!mtcars))
    #> [1] 11

While the result is the same, what is going on under the hood is
completely different.
[`list2()`](https://rlang.r-lib.org/dev/reference/list2.md) is a dots
collector that special-cases `!!!` arguments. On the other hand,
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md) operates
on the language and creates a function call containing as many arguments
as there are elements in the spliced list. If you supply a list of size
1e6, [`inject()`](https://rlang.r-lib.org/dev/reference/inject.md) is
creating one million arguments before evaluation. This can be much
slower.

    xs <- rep(list(1), 1e6)

    system.time(
      n_args(!!!xs)
    )
    #>    user  system elapsed
    #>   0.009   0.000   0.009

    system.time(
      inject(n_args(!!!xs))
    )
    #>    user  system elapsed
    #>   0.445   0.012   0.457

The same issue occurs when functions taking dynamic dots are called
inside a data-masking function like
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
The mechanism that enables `!!!` injection in these arguments is the
same as in
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md).

## See also

- [Injecting with !!, !!!, and glue
  syntax](https://rlang.r-lib.org/dev/reference/topic-inject.md)

- [`inject()`](https://rlang.r-lib.org/dev/reference/inject.md)

- [`exec()`](https://rlang.r-lib.org/dev/reference/exec.md)
