#' Force parts of an expression
#'
#' @description
#'
#' It is sometimes useful to force early evaluation of part of an
#' expression before it gets fully evaluated. The tidy eval framework
#' provides several forcing operators for different use cases.
#'
#' - The bang-bang operator `!!` forces a _single_ object. One
#'   common case for `!!` is to substitute an environment-variable
#'   (created with `<-`) with a data-variable (inside a data frame).
#'
#'   ```
#'   library(dplyr)
#'
#'   # The environment variable `var` refers to the data-variable
#'   # `height`
#'   var <- sym("height")
#'
#'   # We force `var`, which substitutes it with `height`
#'   starwars %>%
#'     summarise(avg = mean(!!var, na.rm = TRUE))
#'   ```
#'
#' - The big-bang operator `!!!` forces-splice a _list_ of objects.
#'   The elements of the list are spliced in place, meaning that they
#'   each become one single argument.
#'
#'   ```
#'   vars <- syms(c("height", "mass"))
#'
#'   # Force-splicing is equivalent to supplying the elements separately
#'   starwars %>% select(!!!vars)
#'   starwars %>% select(height, mass)
#'   ```
#'
#' - The curly-curly operator `{{ }}` for function arguments is a bit
#'   special because it forces the function argument and immediately
#'   defuses it. The defused expression is substituted in place, ready
#'   to be evaluated in another context, such as the data frame.
#'
#'   In practice, this is useful when you have a data-variable in an
#'   env-variable (such as a function argument).
#'
#'   ```
#'   # Force-defuse all function arguments that might contain
#'   # data-variables by embracing them with {{ }}
#'   mean_by <- function(data, by, var) {
#'     data %>%
#'       group_by({{ by }}) %>%
#'       summarise(avg = mean({{ var }}, na.rm = TRUE))
#'   }
#'
#'   # The env-variables `by` and `var` are forced but defused.
#'   # The data-variables they contain are evaluated by dplyr later on
#'   # in data context.
#'   iris %>% mean_by(by = Species, var = Sepal.Width)
#'   ```
#'
#' Use `qq_show()` to experiment with forcing operators. `qq_show()`
#' defuses its input, processes all forcing operators, and prints the
#' result with [expr_print()] to reveal objects inlined in the
#' expression by the forcing operators.
#'
#'
#' @section Forcing names:
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
#' more flexible you can also force names on its LHS:
#'
#' ```
#' name <- "Jane"
#'
#' list2(!!name := 1 + 2)
#' exprs(!!name := 1 + 2)
#' quos(!!name := 1 + 2)
#' ```
#'
#' Like `=`, the `:=` operator expects strings or symbols on its LHS.
#'
#' Currently, forcing names with `:=` only works in top level
#' expressions. These are all valid:
#'
#' ```
#' exprs(!!nm := x)
#' tibble(!!nm := x)
#' list2(!!nm := x)
#' ```
#'
#' But deep-forcing names isn't supported:
#'
#' ```
#' expr(this(is(deep(!!nm := x))))
#' exprs(this(is(deep(!!nm := x))))
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
#'
#' @section Life cycle:
#'
#' * Calling `UQ()` and `UQS()` with the rlang namespace qualifier is
#'   deprecated as of rlang 0.3.0. Just use the unqualified forms
#'   instead:
#'
#'   ```
#'   # Bad
#'   rlang::expr(mean(rlang::UQ(var) * 100))
#'
#'   # Ok
#'   rlang::expr(mean(UQ(var) * 100))
#'
#'   # Good
#'   rlang::expr(mean(!!var * 100))
#'   ```
#'
#'   Supporting namespace qualifiers complicates the implementation of
#'   unquotation and is misleading as to the nature of unquoting
#'   operators (which are syntactic operators that operates at
#'   quotation-time rather than function calls at evaluation-time).
#'
#' * `UQ()` and `UQS()` were soft-deprecated in rlang 0.2.0 in order
#'   to make the syntax of quasiquotation more consistent. The prefix
#'   forms are now \code{`!!`()} and \code{`!!!`()} which is
#'   consistent with other R operators (e.g. \code{`+`(a, b)} is the
#'   prefix form of `a + b`).
#'
#'   Note that the prefix forms are not as relevant as before because
#'   `!!` now has the right operator precedence, i.e. the same as
#'   unary `-` or `+`. It is thus safe to mingle it with other
#'   operators, e.g. `!!a + !!b` does the right thing. In addition the
#'   parser now strips one level of parentheses around unquoted
#'   expressions. This way `(!!"foo")(...)` expands to `foo(...)`.
#'   These changes make the prefix forms less useful.
#'
#'   Finally, the named functional forms `UQ()` and `UQS()` were
#'   misleading because they suggested that existing knowledge about
#'   functions is applicable to quasiquotation. This was reinforced by
#'   the visible definitions of these functions exported by rlang and
#'   by the tidy eval parser interpreting `rlang::UQ()` as `!!`. In
#'   reality unquoting is *not* a function call, it is a syntactic
#'   operation. The operator form makes it clearer that unquoting is
#'   special.
#'
#' @name nse-force
#' @aliases quasiquotation UQ UQS {{}} \{\{
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
NULL

#' @rdname nse-force
#' @usage NULL
#' @export
UQ <- function(x) {
  abort("`UQ()` can only be used within a quasiquoted argument")
}
#' @rdname nse-force
#' @usage NULL
#' @export
UQS <- function(x) {
  abort("`UQS()` can only be used within a quasiquoted argument")
}
#' @rdname nse-force
#' @usage NULL
#' @export
`!!` <- function(x) {
  abort("`!!` can only be used within a quasiquoted argument")
}
#' @rdname nse-force
#' @usage NULL
#' @export
`!!!` <- function(x) {
  abort("`!!!` can only be used within a quasiquoted argument")
}
#' @rdname nse-force
#' @usage NULL
#' @export
`:=` <- function(x, y) {
  abort("`:=` can only be used within a quasiquoted argument")
}

#' @rdname nse-force
#' @param expr An expression to be quasiquoted.
#' @export
qq_show <- function(expr) {
  expr_print(enexpr(expr))
}

#' Process unquote operators in a captured expression
#'
#' While all capturing functions in the tidy evaluation framework
#' perform unquote on capture (most notably [quo()]),
#' `expr_interp()` manually processes unquoting operators in
#' expressions that are already captured. `expr_interp()` should be
#' called in all user-facing functions expecting a formula as argument
#' to provide the same quasiquotation functionality as NSE functions.
#'
#' @param x A function, raw expression, or formula to interpolate.
#' @param env The environment in which unquoted expressions should be
#'   evaluated. By default, the formula or closure environment if a
#'   formula or a function, or the current environment otherwise.
#' @export
#' @examples
#' # All tidy NSE functions like quo() unquote on capture:
#' quo(list(!!(1 + 2)))
#'
#' # expr_interp() is meant to provide the same functionality when you
#' # have a formula or expression that might contain unquoting
#' # operators:
#' f <- ~list(!!(1 + 2))
#' expr_interp(f)
#'
#' # Note that only the outer formula is unquoted (which is a reason
#' # to use expr_interp() as early as possible in all user-facing
#' # functions):
#' f <- ~list(~!!(1 + 2), !!(1 + 2))
#' expr_interp(f)
#'
#'
#' # Another purpose for expr_interp() is to interpolate a closure's
#' # body. This is useful to inline a function within another. The
#' # important limitation is that all formal arguments of the inlined
#' # function should be defined in the receiving function:
#' other_fn <- function(x) toupper(x)
#'
#' fn <- expr_interp(function(x) {
#'   x <- paste0(x, "_suffix")
#'   !!! body(other_fn)
#' })
#' fn
#' fn("foo")
expr_interp <- function(x, env = NULL) {
  if (is_formula(x)) {
    f_rhs(x) <- .Call(rlang_interp, f_rhs(x), env %||% f_env(x))
  } else if (is_closure(x)) {
    body(x) <- .Call(rlang_interp, body(x), env %||% fn_env(x))
  } else {
    x <- .Call(rlang_interp, x, env %||% parent.frame())
  }
  x
}
