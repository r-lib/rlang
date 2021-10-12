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
#' ambiguity][howto-data-mask-ambiguity], and you can inject [defused
#' expressions][topic-defusal] like [symbolised][sym] column names.
#'
#'
#' @section Where does `!!` work?:
#'
#' `!!` does not work everywhere, you can only use it within certain
#' special functions:
#'
#' -   Functions taking [defused][topic-defusal] and
#'     [data-masked][topic-data-masking] arguments.
#' -   Inside [inject()].
#'
#' All data-masked tidyverse verbs support injection operators out of
#' the box. With base functions you need to use [inject()] to enable
#' `!!`. Using `!!` out of context may lead to incorrect results, see
#' [What happens if I use injection operators out of
#' context?][topic-injection-out-of-context].
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
#' expression][topic-defusal]. In the following example we use the
#' [symbolise-and-inject][howto-symbolise-and-inject] pattern to
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
#' Second, the [embrace operator][embrace-operator] makes the
#' [defuse-and-inject pattern][howto-defuse-and-inject] easier to
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
#' -   Injecting [defused expressions][topic-defusal] like
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
#' -   Functions taking [defused][topic-defusal] and
#'     [data-masked][topic-data-masking] arguments.
#' -   Inside [inject()].
#'
#' Most tidyverse functions support `!!!` out of the box. With base
#' functions you need to use [inject()] to enable `!!!`.
#'
#' Using the operator out of context may lead to incorrect results,
#' see [What happens if I use injection operators out of
#' context?][topic-injection-out-of-context].
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
#' expressions][topic-defusal] into [data-masked][topic-data-masking]
#' dots. However this usage is no longer a common pattern for
#' programming with tidyverse functions and we recommend using other
#' patterns if possible.
#'
#' First, instead of using the [defuse-and-inject
#' pattern][howto-defuse-and-inject] with `...`, you can simply pass
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


#' Injecting with `!!`, `!!!`, and glue syntax
#'
#' @description
#'
#' It is sometimes useful to inject language objects or other kinds of
#' objects inside an expression before it gets fully evaluated. The
#' tidy eval framework provides several injection operators for
#' different use cases.
#'
#' - The [injection operator][injection-operator] `!!` (pronounced
#'   "bang-bang") injects a _single_ object. One common case for `!!`
#'   is to substitute an environment-variable (created with `<-`) with
#'   a data-variable (inside a data frame).
#'
#'   ```
#'   library(dplyr)
#'
#'   # The env-variable `var` contains a symbol object, in this
#'   # case a reference to the data-variable `height`
#'   var <- sym("height")
#'
#'   # We inject the data-variable contained in `var` inside `summarise()` 
#'   starwars %>%
#'     summarise(avg = mean(!!var, na.rm = TRUE))
#'   ```
#'
#' - The [splice operator][splice-operator] `!!!` injects a _list_ of
#'   objects. Whereas `!!` would inject the list itself, `!!!` injects
#'   each element of the list in turn. This is also called "splicing".
#'
#'   ```
#'   vars <- syms(c("height", "mass"))
#'
#'   # Injecting with `!!!` is equivalent to supplying the elements separately
#'   starwars %>% select(!!!vars)
#'   starwars %>% select(height, mass)
#'   ```
#'
#' - The [embracing operator][embrace-operator] `{{` (pronounced
#'   "curly-curly") is made specially for function arguments. It
#'   [defuses][nse-defuse] the argument and immediately injects it in
#'   place. The injected argument can then be evaluated in another
#'   context like a data frame.
#'
#'   ```
#'   # Inject function arguments that might contain
#'   # data-variables by embracing them with {{ }}
#'   mean_by <- function(data, by, var) {
#'     data %>%
#'       group_by({{ by }}) %>%
#'       summarise(avg = mean({{ var }}, na.rm = TRUE))
#'   }
#'
#'   # The data-variables `Species` and `Sepal.Width` inside the
#'   # env-variables `by` and `var` are injected inside `group_by()`
#'   # and `summarise()`
#'   iris %>% mean_by(by = Species, var = Sepal.Width)
#'   ```
#'
#'   Learn more about this pattern in [Embracing and
#'   forwarding][howto-embrace-forward].
#'
#' Use `qq_show()` to experiment with injection operators. `qq_show()`
#' defuses its input, processes all injection operators, and prints
#' the result with [expr_print()] to reveal the injected objects.
#'
#'
#' @section Injecting names:
#'
#' When a function takes multiple named arguments
#' (e.g. `dplyr::mutate()`), it is difficult to supply a variable as
#' name. Since the LHS of `=` is [defused][nse-defuse], giving the name
#' of a variable results in the argument having the name of the
#' variable rather than the name stored in that variable. This problem
#' of forcing evaluation of names is exactly what the `!!` operator is
#' for.
#'
#' Unfortunately R is very strict about the kind of expressions
#' supported on the LHS of `=`. This is why rlang interprets the
#' walrus operator `:=` as an alias of `=`. You can use it to supply
#' names, e.g. `a := b` is equivalent to `a = b`. Since its syntax is
#' more flexible you can also inject names on its LHS:
#'
#' ```
#' name <- "Jane"
#'
#' list2(!!name := 1 + 2)
#' exprs(!!name := 1 + 2)
#' ```
#'
#' Like `=`, the `:=` operator expects strings or symbols on its LHS.
#'
#' Since unquoting names is related to interpolating within a string
#' with the glue package, we have made the glue syntax available on
#' the LHS of `:=`:
#'
#' ```
#' list2("{name}" := 1)
#' tibble("{name}" := 1)
#' ```
#'
#' You can also interpolate defused function arguments with double
#' braces `{{`, similar to the curly-curly syntax:
#'
#' ```
#' wrapper <- function(data, var) {
#'   data %>% mutate("{{ var }}_foo" := {{ var }} * 2)
#' }
#' ```
#'
#' Currently, injecting names with `:=` only works in top level
#' expressions. These are all valid:
#'
#' ```
#' exprs("{name}" := x)
#' tibble("{name}" := x)
#' ```
#'
#' But deep-injection of names isn't supported:
#'
#' ```
#' exprs(this(is(deep("{name}" := x))))
#' ```
#'
#'
#' @section Theory:
#'
#' Formally, `quo()` and `expr()` are quasiquotation functions, `!!`
#' is the unquote operator, and `!!!` is the unquote-splice operator.
#' These terms have a rich history in Lisp languages, and live on in
#' modern languages like
#' [Julia](https://docs.julialang.org/en/v1/manual/metaprogramming/)
#' and
#' [Racket](https://docs.racket-lang.org/reference/quasiquote.html).
#'
#' @examples
#' # Interpolation with {{  }} is the easiest way to forward
#' # arguments to tidy eval functions:
#' if (is_attached("package:dplyr")) {
#'
#' # Forward all arguments involving data frame columns by
#' # interpolating them within other data masked arguments.
#' # Here we interpolate `arg` in a `summarise()` call:
#' my_function <- function(data, arg) {
#'   summarise(data, avg = mean({{ arg }}, na.rm = TRUE))
#' }
#'
#' my_function(mtcars, cyl)
#' my_function(mtcars, cyl * 10)
#'
#' # The  operator is just a shortcut for `!!enquo()`:
#' my_function <- function(data, arg) {
#'   summarise(data, avg = mean(!!enquo(arg), na.rm = TRUE))
#' }
#'
#' my_function(mtcars, cyl)
#'
#' }
#'
#' # Quasiquotation functions quote expressions like base::quote()
#' quote(how_many(this))
#' expr(how_many(this))
#' quo(how_many(this))
#'
#' # In addition, they support unquoting. Let's store symbols
#' # (i.e. object names) in variables:
#' this <- sym("apples")
#' that <- sym("oranges")
#'
#' # With unquotation you can insert the contents of these variables
#' # inside the quoted expression:
#' expr(how_many(!!this))
#' expr(how_many(!!that))
#'
#' # You can also insert values:
#' expr(how_many(!!(1 + 2)))
#' quo(how_many(!!(1 + 2)))
#'
#'
#' # Note that when you unquote complex objects into an expression,
#' # the base R printer may be a bit misleading. For instance compare
#' # the output of `expr()` and `quo()` (which uses a custom printer)
#' # when we unquote an integer vector:
#' expr(how_many(!!(1:10)))
#' quo(how_many(!!(1:10)))
#'
#' # This is why it's often useful to use qq_show() to examine the
#' # result of unquotation operators. It uses the same printer as
#' # quosures but does not return anything:
#' qq_show(how_many(!!(1:10)))
#'
#'
#' # Use `!!!` to add multiple arguments to a function. Its argument
#' # should evaluate to a list or vector:
#' args <- list(1:3, na.rm = TRUE)
#' quo(mean(!!!args))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9, na.rm = TRUE)
#' quo(mean(!!var , !!!extra_args))
#'
#'
#' # The plural versions have support for the `:=` operator.
#' # Like `=`, `:=` creates named arguments:
#' quos(mouse1 := bernard, mouse2 = bianca)
#'
#' # The `:=` is mainly useful to unquote names. Unlike `=` it
#' # supports `!!` on its LHS:
#' var <- "unquote me!"
#' quos(!!var := bernard, mouse2 = bianca)
#'
#'
#' # All these features apply to dots captured by enquos():
#' fn <- function(...) enquos(...)
#' fn(!!!args, !!var := penny)
#'
#'
#' # Unquoting is especially useful for building an expression by
#' # expanding around a variable part (the unquoted part):
#' quo1 <- quo(toupper(foo))
#' quo1
#'
#' quo2 <- quo(paste(!!quo1, bar))
#' quo2
#'
#' quo3 <- quo(list(!!quo2, !!!syms(letters[1:5])))
#' quo3
#'
#' @name topic-injection
#' @aliases quasiquotation nse-force nse-inject
NULL

#' @rdname topic-injection
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
