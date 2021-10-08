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
#' expressions][topic-defuse] like [symbolised][sym] column names.
#'
#'
#' @section Where does `!!` work?:
#'
#' `!!` does not work everywhere, you can only use it within certain
#' special functions:
#'
#' -   Functions taking [defused][topic-defuse] and
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
#' Note that tidyverse verbs (or any data-masking function based
#' on [eval_tidy()]) support the [`.env`][.env] pronoun to
#' disambiguate data- and env-variables.
#'
#'
#' @section Injecting expressions:
#'
#' Injection is also useful for modifying parts of a [defused
#' expression][topic-defuse]. In the following example we use the
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
#' TODO!
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
#' @name injecting
#' @aliases quasiquotation nse-force nse-inject
# TODO! Rename to topic-injection
NULL

#' @rdname injecting
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
