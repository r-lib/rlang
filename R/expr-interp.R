#' Quasiquotation of an expression
#'
#' @description
#'
#' Quasiquotation is the mechanism that makes it possible to program
#' flexibly with
#' [tidyeval](http://rlang.tidyverse.org/articles/tidy-evaluation.html)
#' grammars like dplyr. It is enabled in all tidyeval functions, the
#' most fundamental of which are [quo()] and [expr()].
#'
#' Quasiquotation is the combination of quoting an expression while
#' allowing immediate evaluation (unquoting) of part of that
#' expression. We provide both syntactic operators and functional
#' forms for unquoting.
#'
#' - The `!!` operator unquotes its argument. It gets evaluated
#'   immediately in the surrounding context.
#'
#' - The `!!!` operator unquotes and splices its argument. The
#'   argument should represents a list or a vector. Each element will
#'   be embedded in the surrounding call, i.e. each element is
#'   inserted as an argument. If the vector is named, the names are
#'   used as argument names.
#'
#'
#' @section Unquoting names:
#'
#' When a function takes multiple named arguments
#' (e.g. `dplyr::mutate()`), it is difficult to supply a variable as
#' name. Since the LHS of `=` is quoted, giving the name of a variable
#' results in the argument having the name of the variable rather than
#' the name stored in that variable. This problem is right up the
#' alley for the unquoting operator `!!`. If you were able to unquote
#' the variable when supplying the name, the argument would be named
#' after the content of that variable.
#'
#' Unfortunately R is very strict about the kind of expressions
#' supported on the LHS of `=`. This is why we have made the more
#' flexible `:=` operator an alias of `=`. You can use it to supply
#' names, e.g. `a := b` is equivalent to `a = b`. Since its syntax is
#' more flexible you can unquote on the LHS:
#'
#' ```
#' name <- "Jane"
#'
#' dots_list(!!name := 1 + 2)
#' exprs(!!name := 1 + 2)
#' quos(!!name := 1 + 2)
#' ```
#'
#' Like `=`, the `:=` operator expects strings or symbols on its LHS.
#'
#'
#' @section Theory:
#'
#' Formally, `quo()` and `expr()` are quasiquote functions, `!!` is
#' the unquote operator, and `!!!` is the unquote-splice operator.
#' These terms have a rich history in Lisp languages, and live on in
#' modern languages like
#' [Julia](https://docs.julialang.org/en/stable/manual/metaprogramming/)
#' and
#' [Racket](https://docs.racket-lang.org/reference/quasiquote.html).
#'
#'
#' @section Life cycle:
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
#' * `UQE()` was deprecated in rlang 0.2.0 in order to make the is
#'   deprecated in order to simplify the quasiquotation syntax. You
#'   can replace its use by a combination of `!!` and `get_expr()`.
#'   E.g. `!! get_expr(x)` is equivalent to `UQE(x)`.
#'
#' @param x An expression to unquote.
#' @name quasiquotation
#' @aliases UQ UQE UQS
#' @examples
#' # Quasiquotation functions act like base::quote()
#' quote(foo(bar))
#' expr(foo(bar))
#' quo(foo(bar))
#'
#' # In addition, they support unquoting:
#' expr(foo(!!(1 + 2)))
#' quo(foo(!!(1 + 2)))
#'
#'
#' # Use `!!!` to add multiple arguments to a function. Its argument
#' # should evaluate to a list or vector:
#' args <- list(1:10, na.rm = TRUE)
#' quo(mean(!!!args))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9, na.rm = TRUE)
#' quo(mean(!!var , !!!extra_args))
#'
#'
#' # Unquoting is especially useful for transforming successively a
#' # captured expression:
#' quo <- quo(foo(bar))
#' quo <- quo(inner(!!quo, arg1))
#' quo <- quo(outer(!!quo, !!!syms(letters[1:3])))
#' quo
#'
#' # Since we are building the expression in the same environment, you
#' # can also start with raw expressions and create a quosure in the
#' # very last step to record the dynamic environment:
#' expr <- expr(foo(bar))
#' expr <- expr(inner(!!expr, arg1))
#' quo <- quo(outer(!!expr, !!!syms(letters[1:3])))
#' quo
NULL

#' @rdname quasiquotation
#' @export
UQ <- function(x) {
  abort("`UQ()` can only be used within a quasiquoted argument")
}
#' @rdname quasiquotation
#' @export
UQE <- function(x) {
  warn("`UQE()` is deprecated. Please use `!! get_expr(x)`")
  abort("`UQE()` can only be used within a quasiquoted argument")
}
#' @rdname quasiquotation
#' @export
UQS <- function(x) {
  abort("`UQS()` can only be used within a quasiquoted argument")
}
#' @rdname quasiquotation
#' @export
`!!` <- function(x) {
  abort("`!!` can only be used within a quasiquoted argument")
}
#' @rdname quasiquotation
#' @export
#' @usage NULL
`!!!` <- function(x) {
  abort("`!!!` can only be used within a quasiquoted argument")
}
#' @rdname quasiquotation
#' @param y An R expression that will be given the argument name
#'   supplied to `x`.
#' @export
`:=` <- function(x, y) {
  abort("`:=` can only be used within a quasiquoted argument")
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
