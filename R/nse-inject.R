#' Injection operator `!!`
#'
#' @description
#'
#' The [injection][topic-inject] operator `!!` injects a value or
#' expression inside another expression. In other words, it modifies a
#' piece of code before R evaluates it.
#'
#' There are two main cases for injection. You can inject constant
#' values to work around issues of [scoping
#' ambiguity][topic-data-mask-ambiguity], and you can inject [defused
#' expressions][topic-defuse] like [symbolised][sym] column names.
#'
#'
#' @section Where does `!!` work?:
#'
#' `!!` does not work everywhere, you can only use it within certain
#' special functions:
#'
#' - Functions taking [defused][topic-defuse] and
#'   [data-masked][topic-data-mask] arguments.
#'
#'   Technically, this means function arguments defused with
#'   `r link("{{")` or `en`-prefixed operators like
#'   [enquo()], [enexpr()], etc.
#'
#' - Inside [inject()].
#'
#' All data-masking verbs in the tidyverse support injection operators
#' out of the box. With base functions, you need to use [inject()] to
#' enable `!!`. Using `!!` out of context may lead to incorrect
#' results, see `r link("topic_inject_out_of_context")`.
#'
#' The examples below are built around the base function [with()].
#' Since it's not a tidyverse function we will use [inject()] to enable
#' `!!` usage.
#'
#'
#' @section Injecting values:
#'
#' Data-masking functions like [with()] are handy because you can
#' refer to column names in your computations. This comes at the price
#' of data mask ambiguity: if you have defined an env-variable of the
#' same name as a data-variable, you get a name collisions. This
#' collision is always resolved by giving precedence to the
#' data-variable (it masks the env-variable):
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cyl <- c(100, 110)
#' with(mtcars, mean(cyl))
#' ```
#'
#' The injection operator offers one way of solving this. Use it to
#' inject the env-variable inside the data-masked expression:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' inject(
#'   with(mtcars, mean(!!cyl))
#' )
#' ```
#'
#' Note that the [`.env`] pronoun is a simpler way of solving the
#' ambiguity. See `r link("topic_data_mask_ambiguity")` for more about
#' this.
#'
#'
#' @section Injecting expressions:
#'
#' Injection is also useful for modifying parts of a [defused
#' expression][topic-defuse]. In the following example we use the
#' [symbolise-and-inject pattern][topic-metaprogramming] to
#' inject a column name inside a data-masked expression.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' var <- sym("cyl")
#' inject(
#'   with(mtcars, mean(!!var))
#' )
#' ```
#'
#' Since [with()] is a base function, you can't inject
#' [quosures][topic-quosure], only naked symbols and calls. This
#' isn't a problem here because we're injecting the name of a data
#' frame column. If the environment is important, try injecting a
#' pre-computed value instead.
#'
#'
#' @section When do I need `!!`?:
#'
#' With tidyverse APIs, injecting expressions with `!!` is no longer a
#' common pattern. First, the [`.env`][.env] pronoun solves the
#' ambiguity problem in a more intuitive way:
#'
#' ```r
#' cyl <- 100
#' mtcars %>% dplyr::mutate(cyl = cyl * .env$cyl)
#' ```
#'
#' Second, the embrace operator `r link("{{")` makes the
#' [defuse-and-inject pattern][topic-metaprogramming] easier to
#' learn and use.
#'
#' ```r
#' my_mean <- function(data, var) {
#'   data %>% dplyr::summarise(mean({{ var }}))
#' }
#'
#' # Equivalent to
#' my_mean <- function(data, var) {
#'   data %>% dplyr::summarise(mean(!!enquo(var)))
#' }
#' ```
#'
#' `!!` is a good tool to learn for advanced applications but our
#' hope is that it isn't needed for common data analysis cases.
#'
#'
#' @seealso
#' - `r link("topic_inject")`
#' - `r link("topic_metaprogramming")`
#'
#' @name injection-operator
#' @aliases bang-bang
NULL

#' @rdname injection-operator
#' @usage NULL
#' @export
`!!` <- function(x) {
  abort("`!!` can only be used within a defused argument.", call = caller_env())
}


#' Splice operator `!!!`
#'
#' @description
#'
#' The splice operator `!!!` implemented in [dynamic dots][dyn-dots]
#' injects a list of arguments into a function call. It belongs to the
#' family of [injection][topic-inject] operators and provides the same
#' functionality as [do.call()].
#'
#' The two main cases for splice injection are:
#'
#' -   Turning a list of inputs into distinct arguments. This is
#'     especially useful with functions that take data in `...`, such as
#'     [base::rbind()].
#'
#'     ```r
#'     dfs <- list(mtcars, mtcars)
#'     inject(rbind(!!!dfs))
#'     ```
#'
#' -   Injecting [defused expressions][topic-defuse] like
#'     [symbolised][sym] column names.
#'
#'     For tidyverse APIs, this second case is no longer as useful
#'     since dplyr 1.0 and the `across()` operator.
#'
#'
#' @section Where does `!!!` work?:
#'
#' `!!!` does not work everywhere, you can only use it within certain
#' special functions:
#'
#' -   Functions taking [dynamic dots][dyn-dots] like [list2()].
#' -   Functions taking [defused][topic-defuse] and
#'     [data-masked][topic-data-mask] arguments, which are dynamic by
#'     default.
#' -   Inside [inject()].
#'
#' Most tidyverse functions support `!!!` out of the box. With base
#' functions you need to use [inject()] to enable `!!!`.
#'
#' Using the operator out of context may lead to incorrect results,
#' see `r link("topic_inject_out_of_context")`.
#'
#'
#' @section Splicing a list of arguments:
#'
#' Take a function like [base::rbind()] that takes data in `...`. This
#' sort of functions takes a variable number of arguments.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' df1 <- data.frame(x = 1)
#' df2 <- data.frame(x = 2)
#'
#' rbind(df1, df2)
#' ```
#'
#' Passing individual arguments is only possible for a fixed amount of
#' arguments. When the arguments are in a list whose length is
#' variable (and potentially very large), we need a programmatic
#' approach like the splicing syntax `!!!`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' dfs <- list(df1, df2)
#'
#' inject(rbind(!!!dfs))
#' ```
#'
#' Because `rbind()` is a base function we used [inject()] to
#' explicitly enable `!!!`. However, many functions implement [dynamic
#' dots][list2] with `!!!` implicitly enabled out of the box.
#'
#' ```{r, include = FALSE}
#' # Work around pkgload collision messages
#' is_installed("tidyr")
#' ````
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tidyr::expand_grid(x = 1:2, y = c("a", "b"))
#'
#' xs <- list(x = 1:2, y = c("a", "b"))
#' tidyr::expand_grid(!!!xs)
#' ```
#'
#' Note how the expanded grid has the right column names. That's
#' because we spliced a _named_ list. Splicing causes each name of the
#' list to become an argument name.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tidyr::expand_grid(!!!set_names(xs, toupper))
#' ```
#'
#'
#' @section Splicing a list of expressions:
#'
#' Another usage for `!!!` is to inject [defused
#' expressions][topic-defuse] into [data-masked][topic-data-mask]
#' dots. However this usage is no longer a common pattern for
#' programming with tidyverse functions and we recommend using other
#' patterns if possible.
#'
#' First, instead of using the [defuse-and-inject
#' pattern][topic-data-mask-programming] with `...`, you can simply pass
#' them on as you normally would. These two expressions are completely
#' equivalent:
#'
#' ```r
#' my_group_by <- function(.data, ...) {
#'   .data %>% dplyr::group_by(!!!enquos(...))
#' }
#'
#' # This equivalent syntax is preferred
#' my_group_by <- function(.data, ...) {
#'   .data %>% dplyr::group_by(...)
#' }
#' ```
#'
#' Second, more complex applications such as [transformation
#' patterns][topic-metaprogramming] can be solved with the `across()`
#' operation introduced in dplyr 1.0. Say you want to take the
#' `mean()` of all expressions in `...`. Before `across()`, you had to
#' defuse the `...` expressions, wrap them in a call to `mean()`, and
#' inject them in `summarise()`.
#'
#' ```r
#' my_mean <- function(.data, ...) {
#'   # Defuse dots and auto-name them
#'   exprs <- enquos(..., .named = TRUE)
#'
#'   # Wrap the expressions in a call to `mean()`
#'   exprs <- purrr::map(exprs, ~ call("mean", .x, na.rm = TRUE))
#'
#'   # Inject them
#'   .data %>% dplyr::summarise(!!!exprs)
#' }
#' ```
#'
#' It is much easier to use `across()` instead:
#'
#' ```r
#' my_mean <- function(.data, ...) {
#'   .data %>% dplyr::summarise(across(c(...), ~ mean(.x, na.rm = TRUE)))
#' }
#' ```
#'
#'
#' @section Performance of injected dots and dynamic dots:
#'
#' Take this [dynamic dots][dyn-dots] function:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' n_args <- function(...) {
#'   length(list2(...))
#' }
#' ```
#'
#' Because it takes dynamic dots you can splice with `!!!` out of the
#' box.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' n_args(1, 2)
#'
#' n_args(!!!mtcars)
#' ```
#'
#' Equivalently you could enable `!!!` explicitly with [inject()].
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' inject(n_args(!!!mtcars))
#' ```
#'
#' While the result is the same, what is going on under the hood is
#' completely different. [list2()] is a dots collector that
#' special-cases `!!!` arguments. On the other hand, [inject()]
#' operates on the language and creates a function call containing as
#' many arguments as there are elements in the spliced list. If you
#' supply a list of size 1e6, `inject()` is creating one million
#' arguments before evaluation. This can be much slower.
#'
#' ```r
#' xs <- rep(list(1), 1e6)
#'
#' system.time(
#'   n_args(!!!xs)
#' )
#' #>    user  system elapsed
#' #>   0.009   0.000   0.009
#'
#' system.time(
#'   inject(n_args(!!!xs))
#' )
#' #>    user  system elapsed
#' #>   0.445   0.012   0.457
#' ```
#'
#' The same issue occurs when functions taking dynamic dots are called
#' inside a data-masking function like `dplyr::mutate()`. The
#' mechanism that enables `!!!` injection in these arguments is the
#' same as in `inject()`.
#'
#' @seealso
#' - `r link("topic_inject")`
#' - [inject()]
#' - [exec()]
#'
#' @name splice-operator
NULL

#' @rdname splice-operator
#' @usage NULL
#' @export
`!!!` <- function(x) {
  abort("`!!!` can only be used within dynamic dots.", call = caller_env())
}


#' Name injection with `"{"` and `"{{"`
#'
#' ```{r, child = "man/rmd/glue-operators.Rmd"}
#' ```
#'
#' @name glue-operators
NULL

#' Defuse function arguments with glue
#'
#' @description
#' `englue()` creates a string with the [glue
#' operators][glue-operators] `{` and `{{`. These operators are
#' normally used to inject names within [dynamic dots][dyn-dots].
#' `englue()` makes them available anywhere within a function.
#'
#' `englue()` must be used inside a function. `englue("{{ var }}")`
#' [defuses][topic-defuse] the argument `var` and transforms it to a
#' string using the default name operation.
#'
#' @param x A string to interpolate with glue operators.
#' @param env User environment where the interpolation data lives in
#'   case you're wrapping `englue()` in another function.
#' @inheritParams args_error_context
#'
#' @details
#' `englue("{{ var }}")` is equivalent to `as_label(enquo(var))`. It
#' [defuses][topic-defuse] `arg` and transforms the expression to a
#' string with [as_label()].
#'
#' In dynamic dots, using only `{` is allowed. In `englue()` you must
#' use `{{` at least once. Use `glue::glue()` for simple
#' interpolation.
#'
#' Before using `englue()` in a package, first ensure that glue is
#' installed by adding it to your `Imports:` section.
#'
#' ```r
#' usethis::use_package("glue", "Imports")
#' ```
#'
#' @section Wrapping `englue()`:
#'
#' You can provide englue semantics to a user provided string by supplying `env`.
#' In this example we create a variant of `englue()` that supports a
#' special `.qux` pronoun by:
#'
#' - Creating an environment `masked_env` that inherits from the user
#'   env, the one where their data lives.
#'
#' - Overriding the `error_arg` and `error_call` arguments to point to
#'   our own argument name and call environment. This pattern is
#'   slightly different from usual error context passing because
#'   `englue()` is a backend function that uses its own error context
#'   by default (and not a checking function that uses _your_ error
#'   context by default).
#'
#' ```{r}
#' my_englue <- function(text) {
#'   masked_env <- env(caller_env(), .qux = "QUX")
#'
#'   englue(
#'     text,
#'     env = masked_env,
#'     error_arg = "text",
#'     error_call = current_env()
#'   )
#' }
#'
#' # Users can then use your wrapper as they would use `englue()`:
#' fn <- function(x) {
#'   foo <- "FOO"
#'   my_englue("{{ x }}_{.qux}_{foo}")
#' }
#'
#' fn(bar)
#' ```
#'
#' If you are creating a low level package on top of englue(), you
#' should also consider exposing `env`, `error_arg` and `error_call`
#' in your `englue()` wrapper so users can wrap your wrapper.
#'
#' @seealso
#' - `r link("topic_inject")`
#'
#' @examples
#' g <- function(var) englue("{{ var }}")
#' g(cyl)
#' g(1 + 1)
#' g(!!letters)
#'
#' # These are equivalent to
#' as_label(quote(cyl))
#' as_label(quote(1 + 1))
#' as_label(letters)
#'
#' @export
englue <- function(x,
                   env = caller_env(),
                   error_call = current_env(),
                   error_arg = "x") {
  check_string(x, arg = error_arg, call = error_call)
  glue_embrace(x, env = env)
}

glue_embrace <- function(text, env = caller_env()) {
  out <- glue_first_pass(text, env = env)
  out <- unstructure(glue::glue(out, .envir = env))

  if (length(out) != 1) {
    msg <- sprintf(
      "The glue string must be size 1, not %s.",
      length(out)
    )
    abort(msg, call = quote(englue()))
  }

  out
}

glue_first_pass <- function(text, env = caller_env()) {
  glue::glue(
    text,
    .open = "{{",
    .close = "}}",
    .transformer = function(...) glue_first_pass_eval(...),
    .envir = env
  )
}
glue_first_pass_eval <- function(text, env) {
  text_expr <- parse_expr(text)
  defused_expr <- eval_bare(call2(enexpr, text_expr), env)

  if (is_symbol(text_expr) && is_missing(defused_expr)) {
    error_arg <- as_string(text_expr)
    error_call <- NULL

    # Find the relevant error frame. There are edge cases where this
    # will not be correct but passing the user error call makes things
    # too complex in the wrapping case.
    while (!identical(env, empty_env())) {
      if (env_has(env, error_arg)) {
        error_call <- env
        break
      }
      env <- env_parent(env)
    }

    check_required(
      defused_expr,
      arg = error_arg,
      call = error_call
    )
  }

  as_label(defused_expr)
}

#' Show injected expression
#'
#' `qq_show()` helps examining [injected expressions][topic-inject]
#' inside a function. This is useful for learning about injection and
#' for debugging injection code.
#'
#' @usage NULL
#' @param expr An expression involving [injection
#'   operators][topic-inject].
#'
#' @section Examples:
#'
#' ```{r, child = "man/rmd/setup.Rmd", include = FALSE}
#' ```
#'
#' `qq_show()` shows the intermediary expression before it is
#' evaluated by R:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' list2(!!!1:3)
#'
#' qq_show(list2(!!!1:3))
#' ```
#' 
#' It is especially useful inside functions to reveal what an injected
#' expression looks like:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' my_mean <- function(data, var) {
#'   qq_show(data %>% dplyr::summarise(mean({{ var }})))
#' }
#'
#' mtcars %>% my_mean(cyl)
#' ```
#'
#' @seealso
#' - `r link("topic_inject")`
#'
#' @export
qq_show <- function(expr) {
  expr_print(enexpr(expr))
}
