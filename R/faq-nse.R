#' What is data-masking and why do I need embracing with `{{`?
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' Data-masking is a distinctive feature of R whereby programming is
#' performed directly on a data set, with columns defined as normal
#' objects.
#'
#' ```{r, error = TRUE, comment = "#>", collapse = TRUE}
#' # Unmasked programming
#' mean(mtcars$cyl + mtcars$am)
#'
#' # Referring to columns is an error - Where is the data?
#' mean(cyl + am)
#'
#' # Data-masking
#' with(mtcars, mean(cyl + am))
#' ```
#'
#' While data-masking makes it easy to program interactively with data
#' frames, it makes it harder to create functions. Passing data-masked
#' arguments to functions requires injection with `{{` (known as
#' [embracing][embracing]) or, in more complex cases, `!!`.
#'
#'
#' @section Why does data-masking require embracing and injection?:
#'
#' Injection (also known as quasiquotation) is a metaprogramming
#' feature that allows you to modify parts of a program. This is
#' needed because under the hood data-masking works by
#' [defusing][defusing] R code to prevent its immediate evaluation.
#' The defused code is resumed later on in a context where data frame
#' columns are defined.
#'
#' ```{r, error = TRUE, comment = "#>", collapse = TRUE}
#' my_mean <- function(data, var1, var2) {
#'   dplyr::summarise(data, mean(var1 + var2))
#' }
#'
#' my_mean(mtcars, cyl, am)
#' ```
#'
#' The problem here is that `summarise()` defuses the R code it was
#' supplied, i.e. `mean(var1 + var2)`.  Instead we want it to see
#' `mean(cyl + am)`. This is why we need metaprogramming, we need to
#' modify that piece of code by injecting the code supplied to the
#' function in place of `var1` and `var2`. The easiest way to achieve
#' that is with `{{`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' my_mean <- function(data, var1, var2) {
#'   dplyr::summarise(data, mean({{ var1 }} + {{ var2 }}))
#' }
#'
#' my_mean(mtcars, cyl, am)
#' ```
#'
#' Technically, `{{` performs two steps: defusal and injection. It
#' defuses the code supplied to our own function and injects it back
#' into another function. This defuse-and-inject pattern can also be
#' performed in distinct steps with [enquo()] and [`!!`][injecting].
#'
#'
#' @section What does "masking" mean?:
#'
#' In normal programming you define objects in the current
#' environment, for instance in the global environment or the
#' environment of the function you are writing.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' factor <- 1000
#'
#' # Can now use `factor` in computations
#' mean(mtcars$cyl * factor)
#' ```
#'
#' This environment also contains all functions currently in scope. In
#' a script this includes the functions attached with `library()`
#' calls; in a package, the functions imported from other packages.
#'
#' If evaluation was performed in a data frame only, we'd lose track
#' of these objects and functions necessary to perform computations.
#' Hence the data are included in a chain of environments where it
#' comes first and has precedence over the user environment. In other
#' words, it _masks_ the user environment.
#'
#' The easiest way to see this is to define an object with the same
#' name as a column. The latter has precedence because data-variables
#' come before env-variables:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Defining an env-variable
#' cyl <- 1000
#'
#' # Referring to a data-variable
#' dplyr::summarise(mtcars, mean(cyl))
#' ```
#'
#' Note that the tidy eval framework provides [pronouns][.data] to
#' disambiguate between the mask and user contexts. It is often a good
#' idea to use these pronouns in production code. The `.env` pronoun
#' is particularly useful when working with abritrary data frames to
#' prevent a column from inadvertently masking a user variable.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cyl <- 1000
#'
#' mtcars %>%
#'   dplyr::summarise(
#'     mean(.data$cyl),
#'     mean(.env$cyl)
#'   )
#' ```
#'
#'
#' @section How does data-masking work?:
#'
#' Data-masking relies on three language features:
#'
#' - [Argument defusing][defusing] (also known as quoting or NSE) with
#'   [substitute()] (base R) or [enquo()] and [enquos()] (rlang). R
#'   code needs to be defused so it can be evaluated later on in a
#'   modified context.
#'
#' - First class environments. Environments are a special type of
#'   list-like object in which defused R code can be evaluated.  The
#'   named elements in an environment define objects. Lists and data
#'   frames can be transformed to environments:
#'
#'   ```
#'   as.environment(mtcars)
#'   #> <environment: 0x7febb17e3468>
#'   ```
#'
#' - Explicit evaluation with [eval()] (base) or [eval_tidy()]
#'   (rlang). When R code is defused, evaluation is interrupted. It
#'   can be resumed later on with [eval()]:
#'
#'   ```{r, comment = "#>", collapse = TRUE}
#'   expr(1 + 1)
#'
#'   eval(expr(1 + 1))
#'   ```
#'
#'   By default `eval()` and `eval_tidy()` evaluate in the current
#'   environment.
#'
#'   ```{r, error = TRUE, comment = "#>", collapse = TRUE}
#'   code <- expr(mean(cyl + am))
#'   eval(code)
#'   ```
#'
#'   You can supply an optional list or data frame that will be
#'   converted to an environment.
#'
#'   ```{r, comment = "#>", collapse = TRUE}
#'   eval(code, mtcars)
#'   ```
#'
#'   Evaluation of defused code then occurs in the context of a data
#'   mask.
#'
#'
#' @section History:
#'
#' The tidyverse embraced the data-masking approach in packages like
#' ggplot2 and dplyr and eventually developed its own programming
#' framework in the rlang package. None of this would have been
#' possible without the following landmark developments from S and R
#' authors.
#'
#' - The S language introduced data scopes with [attach()] (Becker,
#'   Chambers and Wilks, The New S Language, 1988).
#'
#' - The S language introduced data-masked formulas in modelling
#'   functions (Chambers and Hastie, 1993).
#'
#' - Peter Dalgaard (R team) wrote the frametools package in 1997. It
#'   was later included in R as [base::transform()] and
#'   [base::subset()]. This API is an important source of inspiration
#'   for the dplyr package. It was also the first apparition of
#'   _selections_, a variant of data-masking extended and codified
#'   later on in the [tidyselect
#'   package](https://tidyselect.r-lib.org/articles/syntax.html).
#'
#' - In 2000 Luke Tierney (R team) [changed
#'   formulas](https://github.com/wch/r-source/commit/a945ac8e) to
#'   keep track of their original environments. This change published
#'   in R 1.1.0 was a crucial step towards hygienic data masking,
#'   i.e. the proper resolution of symbols in their original
#'   environments. Quosures were inspired by the environment-tracking
#'   mechanism of formulas.
#'
#' - Luke introduced [base::with()] in 2001.
#'
#' - In 2006 the [data.table package](https://r-datatable.com)
#'   included data-masking and selections in the `i` and `j` arguments
#'   of the `[` method of a data frame.
#'
#' - The [dplyr package](https://dplyr.tidyverse.org/) was published
#'   in 2014.
#'
#' - The rlang package developed tidy eval in 2017 as the data-masking
#'   framework of the tidyverse. It introduced the notions of
#'   [quosure][faq-quosure], [implicit injection][injecting] with `!!`
#'   and `!!!`, and [data pronouns][.data].
#'
#' - In 2019, injection with `{{` was introduced in [rlang
#'   0.4.0](https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/) to
#'   simplify the defuse-and-inject pattern. This operator allows
#'   beginners to transport data-masked arguments across functions
#'   more intuitively.
#'
#' @name faq-data-masking
NULL


#' What are quosures and when are they needed?
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' A quosure is a special type of [defused expression][defusing] that
#' keeps track of the original context the expression was written in.
#' The tracking capabilities of quosures is important when interfacing
#' [data-masking functions] together because the functions might come
#' from two unrelated environments, like two different packages.
#'
#' Let's take an example where the R user calls the function
#' `summarise_bmi()` from the foo package to summarise a data frame
#' with statistics of a BMI value. Because the `height` variable of
#' their data frame is not in metres, they use a custom function
#' `div100()` to rescale the column.
#'
#' ```r
#' # Global environment of user
#'
#' div100 <- function(x) {
#'   x / 100
#' }
#'
#' dplyr::starwars %>%
#'   foo::summarise_bmi(mass, div100(height))
#' ```
#'
#' The `summarise_bmi()` function is a data-masking function defined
#' in the namespace of the foo package which looks like this:
#'
#' ```r
#' # Namespace of package foo
#'
#' bmi <- function(mass, height) {
#'   mass / height^2
#' }
#'
#' summarise_bmi <- function(data, mass, height) {
#'   data %>%
#'     bar::summarise_stats(bmi({{ mass }}, {{ height }}))
#' }
#' ```
#'
#' The foo package uses the custom function `bmi()` to perform a
#' computation on two vectors. It interfaces with `summarise_stats()`
#' defined in bar, another package whose namespace looks like this:
#'
#' ```r
#' # Namespace of package bar
#'
#' check_numeric <- function(x) {
#'   stopifnot(is.numeric(x))
#'   x
#' }
#'
#' summarise_stats <- function(data, var) {
#'   data %>%
#'     dplyr::transmute(
#'       var = check_numeric({{ var }})
#'     ) %>%
#'     dplyr::summarise(
#'       mean = mean(var, na.rm = TRUE),
#'       sd = sd(var, na.rm = TRUE)
#'     )
#' }
#' ```
#'
#' Again the package bar uses a custom function, `check_numeric()`, to
#' validate its input. It also interfaces with data-masking functions
#' from dplyr (using the [define-a-constant][faq-double-evaluation]
#' trick to avoid issues of double evaluation).
#'
#' There are three data-masking functions simultaneously interfacing
#' in this snippet:
#'
#' - At the bottom, `dplyr::transmute()` takes a data-masked input,
#'   and creates a data frame of a single column named `var`.
#'
#' - Before this, `bar::summarise_stats()` takes a data-masked input
#'   inside `dplyr::transmute()` and checks it is numeric.
#'
#' - And first of all, `foo::summarise_bmi()` takes two data-masked
#'   inputs inside `bar::summarise_stats()` and transforms them to a
#'   single BMI value.
#'
#' There is a fourth context, the global environment where
#' `summarise_bmi()` is called with two columns defined in a data
#' frame, one of which is transformed on the fly with the user
#' function `div100()`.
#'
#' All of these contexts (except to some extent the global
#' environment) contain functions that are private and invisible to
#' foreign functions. Yet, the final expanded data-masked expression
#' that is evaluated down the line looks like this (with caret
#' characters indicating the quosure boundaries):
#'
#' ```r
#' dplyr::transmute(
#'   var = ^check_numeric(^bmi(^mass, ^div100(height)))
#' )
#' ```
#'
#' The role of quosures is to let R know that `check_numeric()` should
#' be found in the bar package, `bmi()` in the foo package, and
#' `div100()` in the global environment.
#'
#'
#' @section Technical description of quosures:
#'
#' A quosure carries two things:
#'
#' - An expression.
#' - An environment.
#'
#' And implements these behaviours:
#'
#' - It is _callable_. Evaluation produces a result.
#'
#'   For historical reasons, [base::eval()] doesn't support quosure
#'   evaluation. Quosures currently require [eval_tidy()]. We would
#'   like to fix this limitation in the future.
#'
#' - It is _hygienic_. It evaluates in the tracked environment.
#'
#' - It is _maskable_. If evaluated in a data mask (currently only
#'   masks created with [eval_tidy()] or [new_data_mask()]), the mask
#'   comes first in scope before the quosure environment.
#'
#'   Conceptually, a quosure inherits from two chains of environments,
#'   the data mask and the user environment. In practice rlang
#'   implements this special scoping by rechaining the top of the
#'   datamask to the quosure environment currently under evaluation.
#'
#' There are similarities between promises (the ones R uses to
#' implement lazy evaluation, not the async expressions from the
#' promises package) and quosures. One important difference is that
#' promises are only evaluated once and cache the result for
#' subsequent evaluation. Quosures behave more like calls and can be
#' evaluated repeatedly, potentially in a different data mask. This
#' property is useful to implement split-apply-combine evaluations.
#'
#' @name faq-quosure
NULL


#' The double evaluation problem
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' One inherent risk to metaprogramming is to evaluate multiple times
#' a piece of code that only appears to be evaluated once. Take this
#' data-masking function which takes a single input and produces two
#' summaries:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summarise_stats <- function(data, var) {
#'   data %>%
#'     dplyr::summarise(
#'       mean = mean({{ var }}),
#'       sd = sd({{ var }})
#'     )
#' }
#'
#' summarise_stats(mtcars, cyl)
#' ```
#'
#' This function is perfectly fine if the user supplies simple column
#' names. However, data-masked arguments may also include
#' _computations_.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summarise_stats(mtcars, cyl * 100)
#' ````
#'
#' Computations may be slow and may produce side effects. For these
#' reasons, they should only be performed as many times as they appear
#' in the code (unless explicitly documented, e.g. once per group with
#' grouped data frames). Let's try again with a more complex computation:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' times100 <- function(x) {
#'   message("Takes a long time...")
#'   Sys.sleep(0.1)
#'
#'   message("And causes side effects such as messages!")
#'   x * 100
#' }
#'
#' summarise_stats(mtcars, times100(cyl))
#' ```
#'
#' Because of the side effects and the long running time, it is clear
#' that `summarise_stats()` evaluates its input twice. This is because
#' we've injected a defused expression in two different places. The
#' data-masked expression created down the line looks like this (with
#' caret signs representing [quosure][faq-quosure] boundaries):
#'
#' ```r
#' dplyr::summarise(
#'   mean = ^mean(^times100(cyl)),
#'   sd = ^sd(^times100(cyl))
#' )
#' ```
#'
#' The `times100(cyl)` expression is evaluated twice, even though it
#' only appears once in the code. We have a double evaluation bug.
#'
#' One simple way to fix it is to assign the defused input to a
#' constant. You can then refer to that constant in the remaining of
#' the code.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summarise_stats <- function(data, var) {
#'   data %>%
#'     dplyr::transmute(
#'       var = {{ var }},
#'     ) %>%
#'     dplyr::summarise(
#'       mean = mean(var),
#'       sd = sd(var)
#'     )
#' }
#' ```
#'
#' The defused input is now evaluated only once because it is injected
#' only once:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summarise_stats(mtcars, times100(cyl))
#' ```
#'
#'
#' @section What about glue strings?:
#'
#' `{{` [embracing in glue strings][faq-glue-injection] doesn't suffer
#' from the double evaluation problem:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summarise_stats <- function(data, var) {
#'   data %>%
#'     dplyr::transmute(
#'       var = {{ var }},
#'     ) %>%
#'     dplyr::summarise(
#'       "mean_{{ var }}" := mean(var),
#'       "sd_{{ var }}" := sd(var)
#'     )
#' }
#'
#' summarise_stats(mtcars, times100(cyl))
#' ```
#'
#' Since a glue string doesn't need the result of an expression, only
#' the original code converted (deparsed) to a string, it doesn't
#' evaluate injected expressions.
#' 
#'
#' @name faq-double-evaluation
NULL


#' Embracing and injecting names with glue
#'
#' @description
#'
#' TODO!
#'
#' @section What is the difference between `{` and `{{`?:
#'
#' @name faq-glue-injection
NULL
