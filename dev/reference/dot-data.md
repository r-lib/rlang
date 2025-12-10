# `.data` and `.env` pronouns

The `.data` and `.env` pronouns make it explicit where to find objects
when programming with
[data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
functions.

    m <- 10
    mtcars %>% mutate(disp = .data$disp * .env$m)

- `.data` retrieves data-variables from the data frame.

- `.env` retrieves env-variables from the environment.

Because the lookup is explicit, there is no ambiguity between both kinds
of variables. Compare:

    disp <- 10
    mtcars %>% mutate(disp = .data$disp * .env$disp)
    mtcars %>% mutate(disp = disp * disp)

Note that `.data` is only a pronoun, it is not a real data frame. This
means that you can't take its names or map a function over the contents
of `.data`. Similarly, `.env` is not an actual R environment. For
instance, it doesn't have a parent and the subsetting operators behave
differently.

## `.data` versus the magrittr pronoun `.`

In a [magrittr pipeline](https://magrittr.tidyverse.org/), `.data` is
not necessarily interchangeable with the magrittr pronoun `.`. With
grouped data frames in particular, `.data` represents the current group
slice whereas the pronoun `.` represents the whole data frame. Always
prefer using `.data` in data-masked context.

## Where does `.data` live?

The `.data` pronoun is automatically created for you by data-masking
functions using the [tidy eval
framework](https://rlang.r-lib.org/dev/reference/eval_tidy.md). You
don't need to import `rlang::.data` or use
[`library(rlang)`](https://rlang.r-lib.org) to work with this pronoun.

However, the `.data` object exported from rlang is useful to import in
your package namespace to avoid a `R CMD check` note when referring to
objects from the data mask. R does not have any way of knowing about the
presence or absence of `.data` in a particular scope so you need to
import it explicitly or equivalently declare it with
`utils::globalVariables(".data")`.

Note that `rlang::.data` is a "fake" pronoun. Do not refer to
`rlang::.data` with the `rlang::` qualifier in data masking code. Use
the unqualified `.data` symbol that is automatically put in scope by
data-masking functions.
