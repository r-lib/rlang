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
#' arguments to functions requires injection with the [embracing
#' operator][embrace-operator] `{{` or, in more complex cases, the
#' [injection operator][injection-operator] `!!`.
#'
#'
#' @section Why does data-masking require embracing and injection?:
#'
#' Injection (also known as quasiquotation) is a metaprogramming
#' feature that allows you to modify parts of a program. This is
#' needed because under the hood data-masking works by
#' [defusing][topic-defusing] R code to prevent its immediate evaluation.
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
#' performed in distinct steps with [enquo()] and
#' [`!!`][injection-operator].
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
#' - [Argument defusing][topic-defusing] (also known as quoting or NSE) with
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
#'   [quosure][topic-quosure], [implicit injection][topic-injection] with `!!`
#'   and `!!!`, and [data pronouns][.data].
#'
#' - In 2019, injection with `{{` was introduced in [rlang
#'   0.4.0](https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/) to
#'   simplify the defuse-and-inject pattern. This operator allows
#'   beginners to transport data-masked arguments across functions
#'   more intuitively.
#'
#' @name topic-data-masking
NULL


#' Defusing R expressions
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' When a piece of R code is defused, R doesn't return its value like
#' it normally would. Instead it returns the expression in a special
#' tree-like object that describes how to compute a value. These
#' defused expressions can be thought of as blueprints or recipes for
#' computing values.
#'
#' We can observe the difference between computing an expression and
#' defusing it with [expr()]. This function defuses the expression it
#' is supplied.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Return the result of `1 + 1`
#' 1 + 1
#'
#' # Return the expression `1 + 1`
#' expr(1 + 1)
#' ```
#'
#' Evaluation of a defused expression can be resumed at any time with
#' [eval()] (see also [eval_tidy()]).
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Return the expression `1 + 1`
#' e <- expr(1 + 1)
#'
#' # Return the result of `1 + 1`
#' eval(e)
#' ```
#'
#' The most common use case for defusing expressions is to resume its
#' evaluation in a [data mask][topic-data-masking]. This makes it
#' possible for the expression to refer to columns of a data frame as
#' if they were regular objects.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' e <- expr(mean(cyl))
#' eval(e, mtcars)
#' ```
#'
#'
#' @section Do I need to know about defused expressions?:
#'
#' As a tidyverse user you will rarely need to defuse expressions
#' manually with `expr()`, and even more rarely need to resume
#' evaluation with [eval()] or [eval_tidy()]. Instead, you call
#' [data-masking functions][topic-data-masking] which take care of
#' defusing your arguments and resuming them in the context of a data
#' mask.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' mtcars %>% dplyr::summarise(
#'   mean(cyl)  # This is defused and data-masked
#' )
#' ```
#'
#' It is important to know that a function defuses its arguments
#' because it requires slightly different methods when called from a
#' function. The main thing is that arguments must be transported with
#' the [embrace operator][embrace-operator] `{{`. It allows the
#' data-masking function to defuse the correct expression.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' my_mean <- function(data, var) {
#'   dplyr::summarise(data, mean = mean({{ var }}))
#' }
#' ```
#'
#' Read more about this in:
#'
#' - [Embrace and forward][howto-embrace-forward]
#' - [What is data-masking and why do I need to embrace?][topic-data-masking]
#'
#'
#' @section The booby trap analogy:
#'
#' The term "defusing" comes from an analogy to the evaluation model
#' in R. As you may know, R uses lazy evaluation. It means that
#' arguments are only evaluated when they are needed in an expression.
#' Let's take two functions, `ignore()` which doesn't do anything with
#' its argument, and `force()` which returns it:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' ignore <- function(arg) NULL
#' force <- function(arg) arg
#'
#' ignore(warning("boom"))
#'
#' force(warning("boom"))
#' ```
#'
#' The warning is only evaluated by the function that triggers
#' evaluation of its argument. Evaluation of arguments can be chained
#' by passing them to other functions. If one of the functions ignores
#' its argument, it breaks the chain of evaluation.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) ignore(z)
#'
#' f(warning("boom"))
#' ```
#'
#' In a way, arguments are like _booby traps_ which explode (evaluate)
#' when touched. Defusing an expression can be seen as defusing the
#' booby trap.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' expr(force(warning("boom")))
#' ```
#'
#'
#' @section Types of defused expressions:
#'
#' * __Calls__, like `f(1, 2, 3)` or `1 + 1` represent the action of
#'   calling a function to compute a new value, such as a vector.
#'
#' * __Symbols__, like `x` or `df`, represent named objects. When the
#'   object pointed to by the symbol was defined in a function or in
#'   the global environment, we call it an environment-variable. When
#'   the object is a column in a data frame, we call it a
#'   data-variable.
#'
#' * __Constants__, like `1` or `NULL`.
#'
#' You can create new call or symbol objects by using the defusing
#' function `expr()`:
#'
#' ```r
#' # Create a symbol representing objects called `foo`
#' expr(foo)
#' #> foo
#'
#' # Create a call representing the computation of the mean of `foo`
#' expr(mean(foo, na.rm = TRUE))
#' #> mean(foo, na.rm = TRUE)
#'
#' # Return a constant
#' expr(1)
#' #> [1] 1
#'
#' expr(NULL)
#' #> NULL
#' ```
#'
#' Defusing is not the only way to create defused expressions. You can
#' also assemble them from data:
#'
#' ```r
#' # Assemble a symbol from a string
#' var <- "foo"
#' sym(var)
#'
#' # Assemble a call from strings, symbols, and constants
#' call("mean", sym(var), na.rm = TRUE)
#' ```
#'
#'
#' @section Local expressions versus function arguments:
#'
#' There are two main ways to defuse expressions, to which correspond
#' two functions in rlang, [expr()] and [enquo()]:
#'
#' * You can defuse your _own_ R expressions with `expr()`.
#'
#' * You can defuse the expressions supplied by _the user_ of your
#'   function with the `en`-prefixed operators, such as `enquo()` and
#'   `enquos()`. These operators defuse function arguments.
#'
#'
#' @section Defuse and inject:
#'
#' One purpose for defusing evaluation of an expression is to
#' interface with [data-masking functions][topic-data-masking] by
#' injecting the expression back into another function with `!!`. This
#' is the [defuse-and-inject pattern][howto-defuse-and-inject].
#'
#' ```r
#' my_summarise <- function(data, arg) {
#'   # Defuse the user expression in `arg`
#'   arg <- enquo(arg)
#'
#'   # Inject the expression contained in `arg`
#'   # inside a `summarise()` argument
#'   data |> dplyr::summarise(mean = mean(!!arg, na.rm = TRUE))
#' }
#' ```
#'
#' Defuse-and-inject is usually performed in a single step with the
#' [embracing operator][embrace-operator] `{{`.
#'
#' ```r
#' my_summarise <- function(data, arg) {
#'   # Defuse and inject in a single step with the embracing operator
#'   data |> dplyr::summarise(mean = mean({{ arg }}, na.rm = TRUE))
#' }
#' ```
#'
#' Using `enquo()` and `!!` separately is useful in more complex
#' cases where you need access to the defused expression instead of
#' just passing it on.
#'
#'
#' @section Defused arguments and quosures:
#'
#' If you inspect the return values of `expr()` and `enquo()`, you'll
#' notice that the latter doesn't return a raw expression like the
#' former. Instead it returns a [quosure], a wrapper containing an
#' expression and an environment.
#'
#' ```r
#' expr(1 + 1)
#' #> 1 + 1
#'
#' my_function <- function(arg) enquo(arg)
#' my_function(1 + 1)
#' #> <quosure>
#' #> expr: ^1 + 1
#' #> env:  global
#' ```
#'
#' R needs information about the environment to properly evaluate
#' argument expressions because they come from a different context
#' than the current function. For instance when a function in your
#' package calls `dplyr::mutate()`, the quosure environment indicates
#' where all the private functions of your package are defined.
#'
#' Read more about the role of quosures in [What are
#' quosures and when are they needed?][topic-quosure].
#'
#'
#' @section Comparison with base R:
#'
#' Defusing is known as _quoting_ in other frameworks.
#'
#' - The equivalent of `expr()` is [base::bquote()].
#'
#' - The equivalent of `enquo()` is [base::substitute()]. The latter
#'   returns a naked expression instead of a quosure.
#'
#' - There is no equivalent for `enquos(...)` but you can defuse dots
#'   as a list of naked expressions with `eval(substitute(alist(...)))`.
#'
#' What makes tidy eval work consistently and safely is that defused
#' argument expressions are wrapped in a [quosure]. Unlike a naked
#' expression, a quosure carries information about the context from
#' which an expression comes from. A quosure is evaluated in the
#' original environment of the expression, which allows R to find
#' local data and local functions.
#'
#' @name topic-defuse
#' @aliases quotation nse-defuse
NULL


#' What are quosures and when are they needed?
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' A quosure is a special type of [defused expression][topic-defusing] that
#' keeps track of the original context the expression was written in.
#' The tracking capabilities of quosures is important when interfacing
#' [data-masking functions][topic-data-masking] together because the
#' functions might come from two unrelated environments, like two
#' different packages.
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
#' from dplyr (using the [define-a-constant][howto-double-evaluation]
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
#' @name topic-quosure
NULL


#' Embracing with `{{` and forwarding `...`
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' Calling [data-masked][topic-data-masking] functions from another
#' function is a bit trickier than regular function calls.
#'
#' -   Individual arguments must be forwarded with `{{`.
#'
#'     ```{r, comment = "#>", collapse = TRUE}
#'     my_mean <- function(data, var) {
#'       data %>% dplyr::summarise(mean({{ var }}, na.rm = TRUE))
#'     }
#'
#'     mtcars %>% my_mean(cyl)
#'     ```
#'
#' -   On the other hand multiple arguments can be forwarded the normal
#'     way with `...`.
#'
#'     ```{r, comment = "#>", collapse = TRUE}
#'     my_mean <- function(.data, ..., .var) {
#'       .data %>%
#'         dplyr::group_by(...) %>%
#'         dplyr::summarise(mean({{ .var }}, na.rm = TRUE))
#'     }
#'
#'     mtcars %>% my_mean(am, vs, .var = cyl)
#'     ```
#'
#' Together, embracing and dots form the main way of writing functions
#' around tidyverse pipelines and [tidy eval][eval_tidy] functions in
#' general. In more complex cases, you might need to
#' [defuse][topic-defusing] variables and dots, and
#' [inject][topic-injection] them back with `!!` and `!!!`.
#'
#' @seealso
#' - [The double evaluation problem][howto-double-evaluation]
#'
#' @name howto-embrace-forward
NULL


#' Defuse and inject pattern
#'
#' TODO!
#'
#' @name howto-defuse-and-inject
NULL


#' Symbolise and inject pattern
#'
#' TODO!
#'
#' @name howto-symbolise-and-inject
NULL


#' The data mask ambiguity
#'
#' TODO!
#'
#' @name howto-data-mask-ambiguity
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
#' caret signs representing [quosure][topic-quosure] boundaries):
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
#' `{{` [embracing in glue strings][howto-glue-injection] doesn't suffer
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
#' @name howto-double-evaluation
NULL


#' Embracing and injecting names with glue
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' Argument names are normally defused constants, so you can't use
#' argument syntax to supply a variable name:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' var <- "foobar"
#' list(var = 1)
#'
#' # Need to assign the name in a separate step
#' set_names(list(1), var)
#' ```
#'
#' In functions that support [dynamic dots][dyn-dots], you can inject
#' names with `!!` or with glue syntax. In these examples we use
#' [list2()], a variant of `list()` that supports glue injection
#' through dynamic dots. With glue syntax, you can interpolate a
#' string in a variable within a name.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' list2("{var}" := 1)
#'
#' list2("prefix_{var}_suffix" := 1)
#' ```
#'
#'
#' @section What is the difference between `{` and `{{`?:
#'
#' Whereas the glue `{` operator interpolates the contents of a
#' variable (either local objects or function arguments), the tidy
#' eval `{{` operator interpolates a [defused][topic-defusing] function
#' argument. You use botth `{` and `{{` in name injection.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' fn <- function(arg) {
#'   list2("{{ arg }}" := arg)
#' }
#'
#' fn(1)
#' fn(1 + 1)
#' ```
#'
#' The `{{` syntax is mainly useful for interfacing with
#' [data-masking][topic-data-masking] functions, to give more informative
#' default names:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' fn <- function(data, var) {
#'   dplyr::summarise(data, "mean_{{ var }}" := mean({{ var }}))
#' }
#'
#' fn(mtcars, cyl)
#'
#' fn(mtcars, am)
#' ```
#'
#' You'll likely find yourself to be quickly limited by this sort of
#' default names for single variables. In the following example the
#' default name is a bit awkward and there is no way for the user to
#' override it.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' fn(mtcars, am + cyl)
#' ```
#'
#' This is functions that take multiple inputs with `...` are often
#' preferred because the user can override default names. If the dots
#' are [dynamic][dyn-dots], they can also use glue syntax.
#'
#'
#' @section Usage of `{{` in wrong contexts:
#'
#' Nothing prevents `{{` from working on regular objects. Ideally it
#' would only work with function arguments and this would be an error:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' var <- "foobar"
#' list2("{{ var }}" := 1)
#' ```
#'
#' Unfortunately, for technical reasons we can't make it fail at the
#' moment. Instead the string `"foobar"` is taken as if it were a
#' defused expression and converted to a string. Notice the extra
#' quotes. The correct glue syntax for interpolating a variable
#' containing a string is single curly embracing:
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' list2("{var}" := 1)
#' ```
#'
#' @name howto-glue-injection
NULL


#' Does `{{` work on regular objects?
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' The [embrace operator][embrace-operator] `{{` should be used
#' exclusively with function arguments:
#'
#' ```r
#' fn <- function(arg) {
#'   quo(foo({{ arg }}))
#' }
#'
#' fn(1 + 1)
#' #> <quosure>
#' #> expr: ^foo(^1 + 1)
#' #> env:  0x7ffd89aac518
#' ```
#'
#' However you may have noticed that it also works on regular objects:
#'
#' ```r
#' fn <- function(arg) {
#'   arg <- force(arg)
#'   quo(foo({{ arg }}))
#' }
#'
#' fn(1 + 1)
#' #> <quosure>
#' #> expr: ^foo(^2)
#' #> env:  0x7ffd8a633398
#' ```
#'
#' In that case, `{{` captures the _value_ of the expression instead
#' of a defused expression. That's because only function arguments can
#' be defused.
#'
#' Note that this issue also applies to [enquo()] (on which `{{` is
#' based).
#'
#'
#' @section Why is this not an error?:
#'
#' Ideally we would have made `{{` on regular objects an error.
#' However this is not possible because in compiled R code it is not
#' always possible to distinguish a regular variable from a function
#' argument. See [Why are strings and other constants enquosed in the
#' empty environment?][topic-embrace-constants] for more about this.
#'
#' @name topic-embrace-non-args
NULL

#' Why are strings and other constants enquosed in the empty environment?
#'
#' @description
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' Function arguments are [defused][topic-defusing] into
#' [quosures][topic-quosure] that keep track of the environment of the
#' defused expression.
#'
#' ```r
#' quo(1 + 1)
#' #> <quosure>
#' #> expr: ^1 + 1
#' #> env:  global
#' ```
#'
#' You might have noticed that when constants are supplied, the
#' quosure tracks the empty environment instead of the current
#' environmnent.
#'
#' ```r
#' quos("foo", 1, NULL)
#' #> <list_of<quosure>>
#' #>
#' #> [[1]]
#' #> <quosure>
#' #> expr: ^"foo"
#' #> env:  empty
#' #>
#' #> [[2]]
#' #> <quosure>
#' #> expr: ^1
#' #> env:  empty
#' #>
#' #> [[3]]
#' #> <quosure>
#' #> expr: ^NULL
#' #> env:  empty
#' ```
#'
#' The reason for this has to do with compilation of R code which
#' makes it impossible to consistently capture environments of
#' constants from function arguments. Argument defusing relies on the
#' _promise_ mechanism of R for lazy evaluation of arguments. When
#' functions are compiled and R notices that an argument is constant,
#' it avoids creating a promise since they slow down function
#' evaluation. Instead, the function is directly supplied a naked
#' constant instead of constant wrapped in a promise.
#'
#'
#' @section Concrete case of promise unwrapping by compilation:
#'
#' We can observe this optimisation by calling into the C-level
#' `findVar()` function to capture promises.
#'
#' ```r
#' # Return the object bound to `arg` without triggering evaluation of
#' # promises
#' f <- function(arg) {
#'   rlang:::find_var(current_env(), sym("arg"))
#' }
#'
#' # Call `f()` with a symbol or with a constant
#' g <- function(symbolic) {
#'   if (symbolic) {
#'     f(letters)
#'   } else {
#'     f("foo")
#'   }
#' }
#'
#' # Make sure these small functions are compiled
#' f <- compiler::cmpfun(f)
#' g <- compiler::cmpfun(g)
#' ```
#'
#' When `f()` is called with a symbolic argument, we get the promise
#' object created by R.
#'
#' ```r
#' g(symbolic = TRUE)
#' #> <promise: 0x7ffd79bac130>
#' ```
#'
#' However, supplying a constant to `"f"` returns the constant
#' directly.
#'
#' ```r
#' g(symbolic = FALSE)
#' #> [1] "foo"
#' ```
#'
#' Without a promise, there is no way to figure out the original
#' environment of an argument.
#'
#'
#' @section Do we need environments for constants?:
#'
#' Data-masking APIs in the tidyverse are intentionally designed so
#' that they don't need an environment for constants.
#'
#' - Data-masking APIs should be able to interpret constants. These
#'   can arise from normal argument passing as we have seen, or by
#'   [injection][topic-injection] with `!!`. There should be no
#'   difference between `dplyr::mutate(mtcars, var = cyl)` and
#'   `dplyr::mutate(mtcars, var = !!mtcars$cyl)`.
#'
#' - Data-masking is an _evaluation_ idiom, not an _introspective_
#'   one. The behaviour of data-masking function should not depend on
#'   the calling environment when a constant (or a symbol evaluating
#'   to a given value) is supplied.
#'
#' @name topic-embrace-constants
NULL

#' What happens if I use injection operators out of context?
#' @description TODO!
#'
#' - `{{` link to [topic-embrace-non-args].
#' @name topic-injection-out-of-context
NULL
