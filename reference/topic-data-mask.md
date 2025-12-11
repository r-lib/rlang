# What is data-masking and why do I need `{{`?

Data-masking is a distinctive feature of R whereby programming is
performed directly on a data set, with columns defined as normal
objects.

    # Unmasked programming
    mean(mtcars$cyl + mtcars$am)
    #> [1] 6.59375

    # Referring to columns is an error - Where is the data?
    mean(cyl + am)
    #> Error:
    #> ! object 'cyl' not found

    # Data-masking
    with(mtcars, mean(cyl + am))
    #> [1] 6.59375

While data-masking makes it easy to program interactively with data
frames, it makes it harder to create functions. Passing data-masked
arguments to functions requires injection with the embracing operator
[`{{`](https://rlang.r-lib.org/reference/embrace-operator.md) or, in
more complex cases, the injection operator
[`!!`](https://rlang.r-lib.org/reference/injection-operator.md).

## Why does data-masking require embracing and injection?

Injection (also known as quasiquotation) is a metaprogramming feature
that allows you to modify parts of a program. This is needed because
under the hood data-masking works by
[defusing](https://rlang.r-lib.org/reference/topic-defuse.md) R code to
prevent its immediate evaluation. The defused code is resumed later on
in a context where data frame columns are defined.

Let's see what happens when we pass arguments to a data-masking function
like `summarise()` in the normal way:

    my_mean <- function(data, var1, var2) {
      dplyr::summarise(data, mean(var1 + var2))
    }

    my_mean(mtcars, cyl, am)
    #> Error in `dplyr::summarise()`:
    #> i In argument: `mean(var1 + var2)`.
    #> Caused by error:
    #> ! object 'cyl' not found

The problem here is that `summarise()` defuses the R code it was
supplied, i.e. `mean(var1 + var2)`. Instead we want it to see
`mean(cyl + am)`. This is why we need injection, we need to modify that
piece of code by injecting the code supplied to the function in place of
`var1` and `var2`.

To inject a function argument in data-masked context, just embrace it
with `{{`:

    my_mean <- function(data, var1, var2) {
      dplyr::summarise(data, mean({{ var1 }} + {{ var2 }}))
    }

    my_mean(mtcars, cyl, am)
    #> # A tibble: 1 x 1
    #>   `mean(cyl + am)`
    #>              <dbl>
    #> 1             6.59

See [Data mask programming
patterns](https://rlang.r-lib.org/reference/topic-data-mask-programming.md)
to learn more about creating functions around data-masking functions.

## What does "masking" mean?

In normal R programming objects are defined in the current environment,
for instance in the global environment or the environment of a function.

    factor <- 1000

    # Can now use `factor` in computations
    mean(mtcars$cyl * factor)
    #> [1] 6187.5

This environment also contains all functions currently in scope. In a
script this includes the functions attached with
[`library()`](https://rdrr.io/r/base/library.html) calls; in a package,
the functions imported from other packages. If evaluation was performed
only in the data frame, we'd lose track of these objects and functions
necessary to perform computations.

To keep these objects and functions in scope, the data frame is inserted
at the bottom of the current chain of environments. It comes first and
has precedence over the user environment. In other words, it *masks* the
user environment.

Since masking blends the data and the user environment by giving
priority to the former, R can sometimes use a data frame column when you
really intended to use a local object.

    # Defining an env-variable
    cyl <- 1000

    # Referring to a data-variable
    dplyr::summarise(mtcars, mean(cyl))
    #> # A tibble: 1 x 1
    #>   `mean(cyl)`
    #>         <dbl>
    #> 1        6.19

The tidy eval framework provides
[pronouns](https://rlang.r-lib.org/reference/dot-data.md) to help
disambiguate between the mask and user contexts. It is often a good idea
to use these pronouns in production code.

    cyl <- 1000

    mtcars %>%
      dplyr::summarise(
        mean_data = mean(.data$cyl),
        mean_env = mean(.env$cyl)
      )
    #> # A tibble: 1 x 2
    #>   mean_data mean_env
    #>       <dbl>    <dbl>
    #> 1      6.19     1000

Read more about this in [The data mask
ambiguity](https://rlang.r-lib.org/reference/topic-data-mask-ambiguity.md).

## How does data-masking work?

Data-masking relies on three language features:

- [Argument defusal](https://rlang.r-lib.org/reference/topic-defuse.md)
  with [`substitute()`](https://rdrr.io/r/base/substitute.html) (base R)
  or [`enquo()`](https://rlang.r-lib.org/reference/enquo.md),
  [`enquos()`](https://rlang.r-lib.org/reference/enquo.md), and
  [`{{`](https://rlang.r-lib.org/reference/embrace-operator.md) (rlang).
  R code is defused so it can be evaluated later on in a special
  environment enriched with data.

- First class environments. Environments are a special type of list-like
  object in which defused R code can be evaluated. The named elements in
  an environment define objects. Lists and data frames can be
  transformed to environments:

      as.environment(mtcars)
      #> <environment: 0x7febb17e3468>

- Explicit evaluation with [`eval()`](https://rdrr.io/r/base/eval.html)
  (base) or
  [`eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.md)
  (rlang). When R code is defused, evaluation is interrupted. It can be
  resumed later on with [`eval()`](https://rdrr.io/r/base/eval.html):

      expr(1 + 1)
      #> 1 + 1

      eval(expr(1 + 1))
      #> [1] 2

  By default [`eval()`](https://rdrr.io/r/base/eval.html) and
  [`eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.md)
  evaluate in the current environment.

      code <- expr(mean(cyl + am))
      eval(code)
      #> Error:
      #> ! object 'am' not found

  You can supply an optional list or data frame that will be converted
  to an environment.

      eval(code, mtcars)
      #> [1] 6.59375

  Evaluation of defused code then occurs in the context of a data mask.

## History

The tidyverse embraced the data-masking approach in packages like
ggplot2 and dplyr and eventually developed its own programming framework
in the rlang package. None of this would have been possible without the
following landmark developments from S and R authors.

- The S language introduced data scopes with
  [`attach()`](https://rdrr.io/r/base/attach.html) (Becker, Chambers and
  Wilks, The New S Language, 1988).

- The S language introduced data-masked formulas in modelling functions
  (Chambers and Hastie, 1993).

- Peter Dalgaard (R team) wrote the frametools package in 1997. It was
  later included in R as
  [`base::transform()`](https://rdrr.io/r/base/transform.html) and
  [`base::subset()`](https://rdrr.io/r/base/subset.html). This API is an
  important source of inspiration for the dplyr package. It was also the
  first apparition of *selections*, a variant of data-masking extended
  and codified later on in the [tidyselect
  package](https://tidyselect.r-lib.org/articles/syntax.html).

- In 2000 Luke Tierney (R team) [changed
  formulas](https://github.com/wch/r-source/commit/a945ac8e) to keep
  track of their original environments. This change published in R 1.1.0
  was a crucial step towards hygienic data masking, i.e. the proper
  resolution of symbols in their original environments. Quosures were
  inspired by the environment-tracking mechanism of formulas.

- Luke introduced [`base::with()`](https://rdrr.io/r/base/with.html) in
  2001.

- In 2006 the [data.table package](https://r-datatable.com) included
  data-masking and selections in the `i` and `j` arguments of the `[`
  method of a data frame.

- The [dplyr package](https://dplyr.tidyverse.org/) was published in
  2014.

- The rlang package developed tidy eval in 2017 as the data-masking
  framework of the tidyverse. It introduced the notions of
  [quosure](https://rlang.r-lib.org/reference/topic-quosure.md),
  [implicit
  injection](https://rlang.r-lib.org/reference/topic-inject.md) with
  `!!` and `!!!`, and [data
  pronouns](https://rlang.r-lib.org/reference/dot-data.md).

- In 2019, injection with `{{` was introduced in [rlang
  0.4.0](https://tidyverse.org/blog/2019/06/rlang-0-4-0/) to simplify
  the defuse-and-inject pattern. This operator allows R programmers to
  transport data-masked arguments across functions more intuitively and
  with minimal boilerplate.

## See also

- [Data mask programming
  patterns](https://rlang.r-lib.org/reference/topic-data-mask-programming.md)

- [Defusing R
  expressions](https://rlang.r-lib.org/reference/topic-defuse.md)
