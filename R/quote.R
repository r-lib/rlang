#' Interpolate a quoted expression.
#'
#' \code{tidy_quote()} and \code{expr_quote()} return their arguments as
#' an unevaluated expression. The former returns a formula, which
#' bundles an expression and a scope, while the latter returns a raw
#' expression. Like all capturing functions in the tidy evaluation
#' framework, the expression is interpolated on capture (see
#' \code{\link{arg_capture}()}) and \code{vignette("tidy-eval")}.
#' Alternatively, \code{tidy_interp()} allows you to interpolate manually
#' when you have constructed a raw expression or formula by yourself.
#'
#' When an expression is interpolated, all sub-expressions within
#' unquoting operators (like \code{UQ(x)} and \code{UQS(x)}) are
#' evaluated and inlined. This provides a powerful mechanism for
#' manipulating expressions. Since unquoting is such an important
#' operation, \code{!!} and \code{!!!} are provided as syntactic
#' shortcuts for unquoting and unquote-splicing (see examples).
#'
#' @section Tidy manipulation of expressions:
#'
#'   Interpolating an expression allows you to inline any value within
#'   the expression. In particular, you can transform a quoted
#'   expression by unquoting another quoted expression into it. The
#'   latter expression gets inlined within the former. This mechanism
#'   allows you to easily program with NSE functions. E.g. \code{var
#'   <- ~baz; tidy_quote(foo(bar, !! var))} produces the formula-quote
#'   \code{~foo(bar, baz)}.
#'
#' @section Tidy evaluation of expressions:
#'
#'   The purpose of a quoted expression is to be eventually evaluated
#'   in a modified scope (e.g. a scope where the columns of a data
#'   frame are directly accessible). While interpolation allows you to
#'   easily manipulate expressions, you need to be a bit careful with
#'   the scope of transplanted subexpressions. If they refer to
#'   variables and functions that are only available in a given
#'   environment (in a given scope), that environment should be
#'   bundled with the expression. That is exactly the purpose of a
#'   one-sided formula: bundling an expression and a scope.
#'
#'   In the general case, i.e. interpolating a function with
#'   \code{tidy_interp()} or capturing a raw expression with
#'   \code{expr_quote()}, interpolation evaluates unquoted expressions
#'   in the calling environment. While useful, raw interpolation does
#'   not allow you to keep track of the scope of an expression.
#'   Interpolating a formula with \code{tidy_interp()} (or during capture
#'   by NSE functions or \code{tidy_quote()}) provides a much more
#'   powerful mechanism. The key is to unquote formulas rather than
#'   raw expressions. The formulas are inlined in the expression and
#'   carry their scope. While such formulas would confuse the base R
#'   function \code{\link[base]{eval}()}, they become self-evaluating
#'   expressions when passed to \code{\link{tidy_eval}()}.
#'
#'   To sum up, one-sided formulas are very much similar to promises
#'   in the tidy evaluation framework. Promises are R objects bound to
#'   function arguments. They make lazy evaluation in R possible: they
#'   bundle an expression (the argument supplied by the caller of the
#'   function) and an environment (the original call site of the
#'   function), and they self-evaluate to return a value as soon as
#'   you touch them. Similarly, formulas self-evaluate when called
#'   within \code{\link{tidy_eval}()}. However, unlike promises, they are
#'   first-class objects: you can pass a formula around and use it to
#'   transform another formula or expression. Formulas are thus
#'   treated as reified promises.
#'
#'   Being able to manipulate a formula has important practical
#'   purposes: you can create them, inspect them interactively, and
#'   modify them (see previous section). Taken together, tidy
#'   modification and tidy evaluation of formulas provide a powerful
#'   mechanism for metaprogramming and programming with DSLs.
#'
#' @section Theory: Formally, \code{tidy_quote()} and \code{expr_quote()}
#'   are quasiquote functions, \code{UQ()} is the unquote operator,
#'   and \code{UQS()} is the unquote splice operator. These terms
#'   have a rich history in LISP, and live on in modern languages like
#'   \href{Julia}{http://docs.julialang.org/en/release-0.1/manual/metaprogramming/} and
#'   \href{Racket}{https://docs.racket-lang.org/reference/quasiquote.html}.
#'
#' @param expr An expression.
#' @param x A function, raw expression, or formula to interpolate.
#'   When interpolating a formula, other formulas are treated as
#'   promises (see section on tidy evaluation).
#' @return For \code{tidy_quote()}, a formula (which contains information
#'   about where to find the objects mentioned in the formula). For
#'   \code{expr_quote()}, a raw quoted expression.
#' @export
#' @aliases UQ UQE UQF UQS
#' @examples
#' # When a tidyeval function captures an argument, it is wrapped in a
#' # formula and interpolated. tidy_quote() is a simple wrapper around
#' # arg_capture() and as such is the fundamental tidyeval
#' # function. It allows you to quote an expression and interpolate
#' # unquoted parts:
#' tidy_quote(foo(bar))
#' tidy_quote(!! 1 + 2)
#' tidy_quote(paste0(!! letters[1:2], "foo"))
#'
#' # Alternatively you can interpolate a formula that is already
#' # constructed:
#' tidy_interp(~!! 1 + 2)
#' f <- ~paste0(!! letters[1:2], "foo")
#' tidy_interp(f)
#'
#' # The !! operator is a syntactic shortcut for unquoting. However
#' # you need to be a bit careful with operator precedence. All
#' # arithmetic and comparison operators bind more tightly than `!`:
#' tidy_interp(x ~ 1 +  !! (1 + 2 + 3) + 10)
#'
#' # For this reason you should always wrap the unquoted expression
#' # with parentheses when operators are involved:
#' tidy_interp(x ~ 1 + (!! 1 + 2 + 3) + 10)
#'
#' # Or you can use the explicit unquote function:
#' tidy_interp(x ~ 1 + UQ(1 + 2 + 3) + 10)
#'
#'
#' # Use !!! or UQS() if you want to add multiple arguments to a
#' # function It must evaluate to a list
#' args <- list(1:10, na.rm = TRUE)
#' tidy_interp(~mean(!!! args))
#' tidy_quote(mean( UQS(args) ))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9, na.rm = TRUE)
#' tidy_quote(mean(UQ(var) , UQS(extra_args)))
#' tidy_interp(~mean(!!var , !!!extra_args))
#'
#'
#' # Unquoting is especially useful for transforming a captured
#' # expression:
#' expr <- quote(foo(bar))
#' expr <- expr_quote(inner(!! expr, arg1))
#' expr <- expr_quote(outer(!! expr, !!! lapply(letters[1:3], as.symbol)))
#' expr
#'
#' # Quasiquotation of formulas is much more powerful. Formulas carry
#' # scope information about the inner expression inlined in the outer
#' # expression upon unquoting. Let's create a formula that quotes a
#' # symbol that only exists in a local scope (a child environment of
#' # the current environment):
#' f1 <- local({ foo <- "foo"; ~foo })
#'
#' # You can evaluate that expression with tidy_eval():
#' tidy_eval(f1)
#'
#' # But you can also inline it in another expression before
#' # evaluation:
#' f2 <- local({ bar <- "bar"; ~toupper(bar)})
#' f3 <- tidy_quote(paste(!!f1, !!f2, "!"))
#' f3
#'
#' # tidy_eval() treats one-sided formulas like promises to be evaluated:
#' tidy_eval(f3)
#'
#'
#' # The formula-promise representation is necessary to preserve scope
#' # information and make sure objects are looked up in the right
#' # place. However, there are situations where it can get in the way.
#' # This is the case when you deal with non-tidy NSE functions that do
#' # not understand formulas. You can inline the RHS of a formula in a
#' # call thanks to the UQE() operator:
#' nse_function <- function(arg) substitute(arg)
#' var <- local(~foo(bar))
#' tidy_quote(nse_function(UQ(var)))
#' tidy_quote(nse_function(UQE(var)))
#'
#' # This is equivalent to unquoting and taking the RHS:
#' tidy_quote(nse_function(!! f_rhs(var)))
#'
#' # One of the most important old-style NSE function is the dollar
#' # operator. You need to use UQE() for subsetting with dollar:
#' var <- ~cyl
#' tidy_quote(mtcars$UQE(var))
#'
#' # `!!`() is also treated as a shortcut. It is meant for situations
#' # where the bang operator would not parse, such as subsetting with
#' # $. Since that's its main purpose, we've made it a shortcut for
#' # UQE() rather than UQ():
#' var <- ~cyl
#' tidy_quote(mtcars$`!!`(var))
#'
#'
#' # Sometimes you would like to unquote an object containing a
#' # formula but include it as is rather than treating it as a
#' # promise. You can use UQF() for this purpose:
#' var <- ~letters[1:2]
#' f <- tidy_quote(list(!!var, UQF(var)))
#' f
#' tidy_eval(f)
#'
#' # Note that two-sided formulas are never treated as fpromises:
#' tidy_eval(tidy_quote(a ~ b))
#'
#'
#' # Finally, you can also interpolate a closure's body. This is
#' # useful to inline a function within another. The important
#' # limitation is that all formal arguments of the inlined function
#' # should be defined in the receiving function:
#' other_fn <- function(x) toupper(x)
#'
#' fn <- tidy_interp(function(x) {
#'   x <- paste0(x, "_suffix")
#'   !!! body(other_fn)
#' })
#' fn
#' fn("foo")
#' @useDynLib rlang interp_
tidy_quote <- function(expr) {
  arg_capture(expr)
}
#' @rdname tidy_quote
#' @export
expr_quote <- function(expr) {
  expr <- substitute(expr)
  .Call(interp_, expr, parent.frame(), FALSE)
}
#' @rdname tidy_quote
#' @export
tidy_interp <- function(x) {
  if (is_formula(x)) {
    f_rhs(x) <- .Call(interp_, f_rhs(x), f_env(x), TRUE)
  } else if (is_closure(x)) {
    body(x) <- .Call(interp_, body(x), fn_env(x), FALSE)
  } else {
    x <- .Call(interp_, x, parent.frame(), FALSE)
  }
  x
}


#' @export
#' @rdname tidy_quote
UQ <- function(x) {
  x
}
#' @export
#' @rdname tidy_quote
UQE <- function(x) {
  if (is_formula(x)) {
    f_rhs(x)
  } else {
    x
  }
}
#' @export
#' @rdname tidy_quote
UQF <- function(x) {
  x
}

#' @export
#' @rdname tidy_quote
UQS <- function(x) {
  if (is_pairlist(x)) {
    x
  } else if (is_vector(x)) {
    as.pairlist(x)
  } else if (inherits(x, "{")) {
    cdr(x)
  } else if (is_lang(x)) {
    pairlist(x)
  } else {
    abort("`x` must be a vector or a language object")
  }
}
