#' Injection operator `!!`
#'
#' @description
#'
#' The injection operator `!!` injects a value or expression inside
#' another expression. In other words, it modifies a piece of code
#' before R evaluates it.
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
#' -   Functions taking [defused][topic-defuse] and
#'     [data-masked][topic-data-mask] arguments.
#' -   Inside [inject()].
#'
#' All data-masked tidyverse verbs support injection operators out of
#' the box. With base functions you need to use [inject()] to enable
#' `!!`. Using `!!` out of context may lead to incorrect results, see
#' [What happens if I use injection operators out of
#' context?][topic-inject-out-of-context].
#'
#' The examples below are built around the base function [with()].
#' Since it's not a tidyverse function we need [inject()] to enable
#' `!!`.
#'
#'
#' @section Injecting values:
#'
#' Data-masking functions like [with()] are handy because you can
#' refer to column names in your computations. This comes at the
#' price of scoping ambiguity: If you have defined an env-variable
#' of the same name as a data-variable, the latter has precedence
#' (it masks the env-variable):
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cyl <- c(100, 110)
#' with(mtcars, mean(cyl))
#' ```
#'
#' To get around this, you can inject the env-variable inside the
#' data-masked expression.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' inject(
#'   with(mtcars, mean(!!cyl))
#' )
#' ```
#'
#'
#' @section Injecting expressions:
#'
#' Injection is also useful for modifying parts of a [defused
#' expression][topic-defuse]. In the following example we use the
#' [symbolise-and-inject pattern][topic-data-mask-programming] to
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
#' Second, the embrace operator [`{{`][embrace-operator] makes the
#' [defuse-and-inject pattern][topic-data-mask-programming] easier to
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
#' - The [splice operator][splice-operator] `!!!`
#' - The [embrace operator][embrace-operator] `{{`
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
#' The splice operator `!!!` injects a list of arguments into a
#' function call. It is the main feature of [dynamic dots][dyn-dots].
#' It modifies a piece of code before R evaluates it by injecting
#' several arguments instead of just one like the
#' [`!!`][injection-operator] operator does.
#'
#' The two main cases for splice injection are:
#'
#' -   Turning a list of inputs into distinct arguments. This is
#'     especially useful with functions that take data in `...`, such as
#'     [base::rbind()].
#'
#'     ```r
#'     dfs <- list(mtcars, mtcars)
#'     inject(rbind(!!!mtcars))
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
#'     [data-masked][topic-data-mask] arguments.
#' -   Inside [inject()].
#'
#' Most tidyverse functions support `!!!` out of the box. With base
#' functions you need to use [inject()] to enable `!!!`.
#'
#' Using the operator out of context may lead to incorrect results,
#' see [What happens if I use injection operators out of
#' context?][topic-inject-out-of-context].
#'
#'
#' @section Splicing a list of arguments:
#'
#' Take a function like [base::rbind()] that takes data in `...`. This
#' sort of functions takes a variable number of arguments.
#'
#' ```{r}
#' df1 <- data.frame(x = 1)
#' df2 <- data.frame(x = 2)
#'
#' rbind(df1, df2)
#' ```
#'
#' This syntax supposes that you are in control of the individual
#' arguments when you are writing the code. When the arguments are in
#' a list whose length is variable, we need another syntax like
#' splicing with `!!!`.
#'
#' ```{r}
#' dfs <- list(df1, df2)
#'
#' inject(rbind(!!!dfs))
#' ```
#'
#' Because `rbind()` is a base function we had to enable `!!!` with
#' [inject()]. However, many functions implement [dynamic dots][list2]
#' out of the box and have `!!!` automatically enabled.
#'
#' ```{r}
#' tidyr::expand_grid(x = 1:2, y = c("a", "b"))
#'
#' xs <- list(x = 1:2, y = c("a", "b"))
#' tidyr::expand_grid(!!!xs)
#' ```
#'
#' Note how the expanded grid has the right column names. That's
#' because the spliced list is named which causes each name of the
#' list to become an argument name.
#'
#' ```{r}
#' tidyr::expand_grid(!!!set_names(xs, toupper))
#' ```
#'
#' Splicing lists of arguments with `!!!` is equivalent to
#' [base::do.call()] (see also [exec()]).
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
#' Second, more complex cases can be solved with the `across()`
#' operation introduced in dplyr 1.0. Say you want to take the
#' `mean()` of all expressions in `...`. Without `across()`, you could
#' defuse the `...` expressions, wrap them in a call to `mean()`, and
#' inject them back in `summarise()`.
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
#' Take a function taking [dynamic dots][dyn-dots] via [list2()]:
#'
#' ```{r}
#' my_function <- function(...) {
#'   xs <- list2(...)
#'
#'   # Do something useful with `xs`
#'   length(xs)
#' }
#' ```
#'
#' Because it takes dynamic dots you can splice with `!!!` out of the
#' box.
#'
#' ```{r}
#' my_function(1, 2)
#'
#' my_function(!!!mtcars)
#' ```
#'
#' Equivalently you could enable `!!!` explicitly with [inject()].
#' 
#' ```{r}
#' inject(my_function(!!!mtcars))
#' ```
#'
#' While the result is the same, what is going on under the hood is
#' completely different. [list2()] is a dots collector that treats
#' `!!!` inputs specially whereas [inject()] operates on the language.
#' Concretely, `inject()` creates a function call containing as many
#' arguments as there are elements in the spliced list. If you supply
#' a list of size 1e6, `inject()` is going to create as many arguments
#' before evaluation. This can be much slower.
#'
#' ```r
#' xs <- rep(list(1), 1e6)
#'
#' system.time(
#'   my_function(!!!xs)
#' )
#' #>    user  system elapsed
#' #>   0.009   0.000   0.009
#'
#' system.time(
#'   inject(my_function(!!!xs))
#' )
#' #>    user  system elapsed
#' #>   0.445   0.012   0.457
#' ```
#'
#' The same issue occurs when functions taking dynamic dots are called
#' inside a data-masking function like `dplyr::mutate()`. The
#' mechanism that enables injection in these arguments is the same as
#' in `inject()`.
#'
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
#' @includeRmd man/rmd/glue-operator.Rmd description 
#'
#' @name glue-operator
NULL

#' Defuse function arguments with glue
#'
#' @description
#' `englue()` creates a string with the [glue
#' operators][glue-operator] `{` and `{{`. These operators are
#' normally used to inject names within [dynamic dots][dyn-dots].
#' `englue()` makes them available anywhere within a function.
#'
#' `englue()` must be used inside a function. `englue("{{ var }}")`
#' [defuses][topic-defuse] the argument `var` and transforms it to a
#' string using the default name operation.
#'
#' @param x A string to interpolate with glue operators.
#'
#' @details
#' `englue("{{ var }}")` is equivalent to `as_label(enquo(var))`. It
#' [defuses][topic-defuse] `arg` and transforms the expression to a
#' string with [as_label()].
#'
#' In dynamic dots, `{` is allowed. In `englue()` you must use `{{`.
#' Use `glue::glue()` for simple interpolation.
#'
#' Before using `englue()` in a package, first ensure that glue is
#' installed by adding it to your `Imports:` section.
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
englue <- function(x) {
  if (!grepl("{{", x, fixed = TRUE)) {
    abort(c(
      "Must use `{{`.",
      i = "Use `glue::glue()` for interpolation with `{`."
    ))
  }

  expr <- call(":=", x, NULL)
  res <- inject(rlang::list2(!!expr), caller_env())
  names(res)
}


#' @rdname topic-inject
#' @param expr An expression involving injection operators.
#' @usage NULL
#' @export
qq_show <- function(expr) {
  expr_print(enexpr(expr))
}


glue_unquote <- function(text, env = caller_env()) {
  glue::glue(glue_first_pass(text, env = env), .envir = env)
}
glue_first_pass <- function(text, env = caller_env()) {
  glue::glue(
    text,
    .open = "{{",
    .close = "}}",
    .transformer = glue_first_pass_eval,
    .envir = env
  )
}
glue_first_pass_eval <- function(text, env) {
  text_expr <- parse_expr(text)
  defused_expr <- eval_bare(call2(enexpr, text_expr), env)
  as_label(defused_expr)
}
