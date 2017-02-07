#' Tidy quotation of an expression.
#'
#' \code{tidy_quote()} captures its argument as an unevaluated
#' expression and returns it as a formula. Formulas are a key part of
#' the tidy evaluation framework because they bundle an expression and
#' a scope (the environment in which \code{tidy_quote()} was called).
#' This means that you can pass a formula around while keeping track
#' of the context where it was created. The symbols quoted in the
#' formula will be evaluated in the right context (where they are
#' likely defined) by \code{\link{tidy_eval}()}.
#'
#' Like all capturing functions in the tidy evaluation framework,
#' \code{tidy_quote()} interpolates on capture (see
#' \code{\link{tidy_capture}()}) and \code{vignette("tidy-eval")}.
#' Alternatively, \code{tidy_interp()} allows you to interpolate
#' manually when you have constructed a raw expression or formula by
#' yourself. When an expression is interpolated, all sub-expressions
#' within unquoting operators (like \code{UQ(x)} and \code{UQS(x)})
#' are evaluated and inlined. This provides a powerful mechanism for
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
#'   In a way, one-sided formulas are very much similar to promises in
#'   the tidy evaluation framework. Promises are R objects bound to
#'   function arguments which make lazy evaluation in R possible: they
#'   bundle an expression (the argument supplied by the caller of the
#'   function) and an environment (the original call site of the
#'   function), and they self-evaluate to return a value as soon as
#'   you touch them. Similarly, formulas self-evaluate when called
#'   within \code{\link{tidy_eval}()}. However, unlike promises, they
#'   are first-class objects: you can pass a formula around and use it
#'   to transform another formula or expression. Formulas are thus
#'   treated as reified promises.
#'
#'   Being able to manipulate a formula has important practical
#'   purposes: you can create them, inspect them interactively, and
#'   modify them (see previous section). Taken together, tidy
#'   modification and tidy evaluation of formulas provide a powerful
#'   mechanism for metaprogramming and programming with DSLs.
#'
#' @section Theory: Formally, \code{tidy_quote()} and \code{tidy_quote_expr()}
#'   are quasiquote functions, \code{UQ()} is the unquote operator,
#'   and \code{UQS()} is the unquote splice operator. These terms
#'   have a rich history in LISP, and live on in modern languages like
#'   \href{Julia}{http://docs.julialang.org/en/release-0.1/manual/metaprogramming/} and
#'   \href{Racket}{https://docs.racket-lang.org/reference/quasiquote.html}.
#'
#' @param expr An expression.
#' @param x An expression to unquote. It is evaluated in the current
#'   environment and inlined in the expression.
#' @return A formula whose right-hand side contains the quoted
#'   expression supplied as argument.
#' @seealso \code{\link{tidy_quote_expr}()} for quoting a raw expression
#'   with quasiquotation, and \code{\link{tidy_interp}()} for
#'   unquoting an already quoted expression or an existing formula.
#' @export
#' @aliases UQ UQE UQF UQS
#' @examples
#' # When a tidyeval function captures an argument, it is wrapped in a
#' # formula and interpolated. tidy_quote() is a simple wrapper around
#' # tidy_capture() and as such is the fundamental tidyeval
#' # function. It allows you to quote an expression and interpolate
#' # unquoted parts:
#' tidy_quote(foo(bar))
#' tidy_quote(1 + 2)
#' tidy_quote(paste0(!! letters[1:2], "foo"))
#'
#' # The !! operator is a syntactic shortcut for unquoting with UQ().
#' # However you need to be a bit careful with operator
#' # precedence. All arithmetic and comparison operators bind more
#' # tightly than `!`:
#' tidy_quote(1 +  !! (1 + 2 + 3) + 10)
#'
#' # For this reason you should always wrap the unquoted expression
#' # with parentheses when operators are involved:
#' tidy_quote(1 + (!! 1 + 2 + 3) + 10)
#'
#' # Or you can use the explicit unquote function:
#' tidy_quote(1 + UQ(1 + 2 + 3) + 10)
#'
#' # Use !!! or UQS() if you want to add multiple arguments to a
#' # function It must evaluate to a list
#' args <- list(1:10, na.rm = TRUE)
#' tidy_quote(mean( UQS(args) ))
#'
#' # You can combine the two
#' var <- quote(xyz)
#' extra_args <- list(trim = 0.9, na.rm = TRUE)
#' tidy_quote(mean(UQ(var) , UQS(extra_args)))
#'
#'
#' # Unquoting is especially useful for transforming a captured
#' # expression:
#' f <- ~foo(bar)
#' f <- tidy_quote(inner(!! f, arg1))
#' f <- tidy_quote(outer(!! f, !!! lapply(letters[1:3], as.symbol)))
#' f
#'
#' # Note that it's fine to unquote formulas as long as you evaluate
#' # with tidy_eval():
#' f <- ~letters
#' f <- tidy_quote(toupper(!! f))
#' tidy_eval(f)
#'
#' # Formulas carry scope information about the inner expression
#' # inlined in the outer expression upon unquoting. To see this,
#' # let's create a formula that quotes a symbol that only exists in a
#' # local scope (a child environment of the current environment):
#' f1 <- local({ foo <- "foo"; ~foo })
#'
#' # You can evaluate that expression with tidy_eval():
#' tidy_eval(f1)
#'
#' # And you can also inline it in another expression before
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
#' @useDynLib rlang rlang_interp
tidy_quote <- function(expr) {
  tidy_capture(expr)
}

#' Untidy quotation of an expression.
#'
#' Unlike \code{\link{tidy_quote}()}, \code{tidy_quote_expr()} returns a
#' raw expression instead of a formula. As a result,
#' \code{tidy_quote_expr()} is untidy in the sense that it does not
#' preserve scope information for the quoted expression. It can still
#' be useful in certain cases. Compared to base R's
#' \code{\link[base]{quote}()}, it unquotes the expression on capture,
#' and compared to \code{\link{tidy_quote}()}, the quoted expression
#' is directly compatible with the base R \code{\link[base]{eval}()}
#' function.
#'
#' @inheritParams tidy_quote
#' @seealso See \code{\link{tidy_quote}()} and
#'   \code{\link{tidy_interp}()} for more explanation on tidy
#'   quotation.
#' @return The raw expression supplied as argument.
#' @export
#' @examples
#' # The advantage of tidy_quote_expr() over quote() is that it unquotes on
#' # capture:
#' tidy_quote_expr(list(1, !! 3 + 10))
#'
#' # Unquoting can be especially useful for successive transformation
#' # of a captured expression:
#' (expr <- quote(foo(bar)))
#' (expr <- tidy_quote_expr(inner(!! expr, arg1)))
#' (expr <- tidy_quote_expr(outer(!! expr, !!! lapply(letters[1:3], as.symbol))))
#'
#' # Unlike tidy_quote(), tidy_quote_expr() produces expressions that can
#' # be evaluated with base::eval():
#' e <- quote(letters)
#' e <- tidy_quote_expr(toupper(!!e))
#' eval(e)
#'
#' # Be careful if you unquote a formula-quote: you need to take the
#' # RHS (and lose the scope information) to evaluate with eval():
#' f <- ~letters
#' e <- tidy_quote_expr(toupper(!! f_rhs(f)))
#' eval(e)
#'
#' # However it's fine to unquote formulas if you evaluate with tidy_eval():
#' f <- ~letters
#' e <- tidy_quote_expr(toupper(!! f))
#' tidy_eval(e)
tidy_quote_expr <- function(expr) {
  expr <- substitute(expr)
  .Call(rlang_interp, expr, parent.frame())
}
