# The double evaluation problem

One inherent risk to metaprogramming is to evaluate multiple times a
piece of code that appears to be evaluated only once. Take this
data-masking function which takes a single input and produces two
summaries:

    summarise_stats <- function(data, var) {
      data %>%
        dplyr::summarise(
          mean = mean({{ var }}),
          sd = sd({{ var }})
        )
    }

    summarise_stats(mtcars, cyl)
    #> # A tibble: 1 x 2
    #>    mean    sd
    #>   <dbl> <dbl>
    #> 1  6.19  1.79

This function is perfectly fine if the user supplies simple column
names. However, data-masked arguments may also include *computations*.

    summarise_stats(mtcars, cyl * 100)
    #> # A tibble: 1 x 2
    #>    mean    sd
    #>   <dbl> <dbl>
    #> 1  619.  179.

Computations may be slow and may produce side effects. For these
reasons, they should only be performed as many times as they appear in
the code (unless explicitly documented, e.g. once per group with grouped
data frames). Let's try again with a more complex computation:

    times100 <- function(x) {
      message("Takes a long time...")
      Sys.sleep(0.1)

      message("And causes side effects such as messages!")
      x * 100
    }

    summarise_stats(mtcars, times100(cyl))
    #> Takes a long time...
    #> And causes side effects such as messages!
    #> Takes a long time...
    #> And causes side effects such as messages!
    #> # A tibble: 1 x 2
    #>    mean    sd
    #>   <dbl> <dbl>
    #> 1  619.  179.

Because of the side effects and the long running time, it is clear that
`summarise_stats()` evaluates its input twice. This is because we've
injected a defused expression in two different places. The data-masked
expression created down the line looks like this (with caret signs
representing
[quosure](https://rlang.r-lib.org/dev/reference/topic-quosure.md)
boundaries):

    dplyr::summarise(
      mean = ^mean(^times100(cyl)),
      sd = ^sd(^times100(cyl))
    )

The `times100(cyl)` expression is evaluated twice, even though it only
appears once in the code. We have a double evaluation bug.

One simple way to fix it is to assign the defused input to a constant.
You can then refer to that constant in the remaining of the code.

    summarise_stats <- function(data, var) {
      data %>%
        dplyr::transmute(
          var = {{ var }},
        ) %>%
        dplyr::summarise(
          mean = mean(var),
          sd = sd(var)
        )
    }

The defused input is now evaluated only once because it is injected only
once:

    summarise_stats(mtcars, times100(cyl))
    #> Takes a long time...
    #> And causes side effects such as messages!
    #> # A tibble: 1 x 2
    #>    mean    sd
    #>   <dbl> <dbl>
    #> 1  619.  179.

## What about glue strings?

`{{` [embracing in glue
strings](https://rlang.r-lib.org/dev/reference/glue-operators.md)
doesn't suffer from the double evaluation problem:

    summarise_stats <- function(data, var) {
      data %>%
        dplyr::transmute(
          var = {{ var }},
        ) %>%
        dplyr::summarise(
          "mean_{{ var }}" := mean(var),
          "sd_{{ var }}" := sd(var)
        )
    }

    summarise_stats(mtcars, times100(cyl))
    #> Takes a long time...
    #> And causes side effects such as messages!
    #> # A tibble: 1 x 2
    #>   `mean_times100(cyl)` `sd_times100(cyl)`
    #>                  <dbl>              <dbl>
    #> 1                 619.               179.

Since a glue string doesn't need the result of an expression, only the
original code converted (deparsed) to a string, it doesn't evaluate
injected expressions.
