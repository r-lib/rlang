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
#' - `UQ()` and the `!!` operator unquote their argument. It gets
#'   evaluated immediately in the surrounding context.
#'
#' - `UQE()` is like `UQ()` but retrieves the expression of
#'   [quosureish][is_quosureish] objects. It is a shortcut for `!!
#'   get_expr(x)`. Use this with care: it is potentially unsafe to
#'   discard the environment of the quosure.
#'
#' - `UQS()` and the `!!!` operators unquote and splice their
#'   argument. The argument should evaluate to a vector or an
#'   expression. Each component of the vector is embedded as its own
#'   argument in the surrounding call. If the vector is named, the
#'   names are used as argument names.
#'
#' @section Theory:
#'
#' Formally, `quo()` and `expr()` are quasiquote functions, `UQ()` is
#' the unquote operator, and `UQS()` is the unquote splice operator.
#' These terms have a rich history in Lisp languages, and live on in
#' modern languages like
#' [Julia](https://docs.julialang.org/en/stable/manual/metaprogramming/)
#' and
#' [Racket](https://docs.racket-lang.org/reference/quasiquote.html).
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
#' expr(foo(UQ(1 + 2)))
#' expr(foo(!! 1 + 2))
#' quo(foo(!! 1 + 2))
#'
#' # The !! operator is a handy syntactic shortcut for unquoting with
#' # UQ().  However you need to be a bit careful with operator
#' # precedence. All arithmetic and comparison operators bind more
#' # tightly than `!`:
#' quo(1 +  !! (1 + 2 + 3) + 10)
#'
#' # For this reason you should always wrap the unquoted expression
#' # with parentheses when operators are involved:
#' quo(1 + (!! 1 + 2 + 3) + 10)
#'
#' # Or you can use the explicit unquote function:
#' quo(1 + UQ(1 + 2 + 3) + 10)
#'
#'
#' # Use !!! or UQS() if you want to add multiple arguments to a
#' # function It must evaluate to a list
#' args <- list(1:10, na.rm = TRUE)
#' quo(mean( UQS(args) ))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9, na.rm = TRUE)
#' quo(mean(UQ(var) , UQS(extra_args)))
#'
#'
#' # Unquoting is especially useful for transforming successively a
#' # captured expression:
#' quo <- quo(foo(bar))
#' quo <- quo(inner(!! quo, arg1))
#' quo <- quo(outer(!! quo, !!! syms(letters[1:3])))
#' quo
#'
#' # Since we are building the expression in the same environment, you
#' # can also start with raw expressions and create a quosure in the
#' # very last step to record the dynamic environment:
#' expr <- expr(foo(bar))
#' expr <- expr(inner(!! expr, arg1))
#' quo <- quo(outer(!! expr, !!! syms(letters[1:3])))
#' quo
NULL

#' @export
#' @rdname quasiquotation
UQ <- function(x) {
  x
}
#' @export
#' @rdname quasiquotation
UQE <- function(x) {
  if (is_quosureish(x)) {
    get_expr(x)
  } else {
    x
  }
}
#' @export
#' @rdname quasiquotation
`!!` <- UQE
#' @export
#' @rdname quasiquotation
UQS <- function(x) {
  if (is_pairlist(x) || is_null(x)) {
    x
  } else if (is_vector(x)) {
    as.pairlist(x)
  } else if (identical(node_car(x), sym_curly)) {
    node_cdr(x)
  } else if (is_expr(x)) {
    pairlist(x)
  } else {
    abort("`x` must be a vector or a language object")
  }
}
#' @export
#' @rdname quasiquotation
#' @usage NULL
`!!!` <- UQS

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
#' quo(list(!! 1 + 2))
#'
#' # expr_interp() is meant to provide the same functionality when you
#' # have a formula or expression that might contain unquoting
#' # operators:
#' f <- ~list(!! 1 + 2)
#' expr_interp(f)
#'
#' # Note that only the outer formula is unquoted (which is a reason
#' # to use expr_interp() as early as possible in all user-facing
#' # functions):
#' f <- ~list(~!! 1 + 2, !! 1 + 2)
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
    expr <- .Call(rlang_interp, f_rhs(x), env %||% f_env(x), TRUE)
    x <- new_quosure(expr, f_env(x))
  } else if (is_closure(x)) {
    body(x) <- .Call(rlang_interp, body(x), env %||% fn_env(x), TRUE)
  } else {
    x <- .Call(rlang_interp, x, env %||% parent.frame(), TRUE)
  }
  x
}
