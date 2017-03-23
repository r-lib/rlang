#' Tidy quotation of an expression.
#'
#' `quo()` captures its argument as an unevaluated expression
#' and returns it as a formula. Formulas are a key part of the tidy
#' evaluation framework because they bundle an expression and a scope
#' (the environment in which `quo()` was called).  This means
#' that you can pass a formula around while keeping track of the
#' context where it was created. The symbols quoted in the formula
#' will be evaluated in the right context (where they are likely
#' defined) by [eval_tidy()].
#'
#' Like all capturing functions in the tidy evaluation framework,
#' `quo()` interpolates on capture (see [enquo()]) and
#' `vignette("tidy-eval")`. Alternatively, `expr_interp()` allows you
#' to interpolate manually when you have constructed a raw expression
#' or formula by yourself. When an expression is interpolated, all
#' sub-expressions within unquoting operators (like `UQ(x)` and
#' `UQS(x)`) are evaluated and inlined. This provides a powerful
#' mechanism for manipulating expressions. Since unquoting is such an
#' important operation, `!!` and `!!!` are provided as syntactic
#' shortcuts for unquoting and unquote-splicing (see examples).
#'
#' @section Tidy manipulation of expressions:
#'
#'   Interpolating an expression allows you to inline any value within
#'   the expression. In particular, you can transform a quoted
#'   expression by unquoting another quoted expression into it. The
#'   latter expression gets inlined within the former. This mechanism
#'   allows you to easily program with NSE functions. E.g. `var <-
#'   ~baz; quo(foo(bar, !! var))` produces the formula-quote
#'   `~foo(bar, baz)`.
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
#'   within [eval_tidy()]. However, unlike promises, they are
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
#' @section Theory: Formally, `quo()` and `expr()`
#'   are quasiquote functions, `UQ()` is the unquote operator, and
#'   `UQS()` is the unquote splice operator. These terms have a rich
#'   history in LISP, and live on in modern languages like
#'   [Julia](http://docs.julialang.org/en/release-0.1/manual/metaprogramming/)
#'   and [Racket](https://docs.racket-lang.org/reference/quasiquote.html).
#'
#' @param expr An expression.
#' @param x An expression to unquote. It is evaluated in the current
#'   environment and inlined in the expression.
#' @return A formula whose right-hand side contains the quoted
#'   expression supplied as argument.
#' @seealso [quos()][quosures] for capturing several expressions,
#'   including from dots; [expr()] for quoting a raw
#'   expression with quasiquotation; and [expr_interp()] for unquoting
#'   an already quoted expression or an existing formula.
#' @export
#' @name quosure
#' @aliases UQ UQE UQF UQS
#' @examples
#' # When a tidyeval function captures an argument, it is wrapped in a
#' # formula and interpolated. quo() is a simple wrapper around
#' # enquo() and as such is the fundamental tidyeval
#' # function. It allows you to quote an expression and interpolate
#' # unquoted parts:
#' quo(foo(bar))
#' quo(1 + 2)
#' quo(paste0(!! letters[1:2], "foo"))
#'
#' # The !! operator is a syntactic shortcut for unquoting with UQ().
#' # However you need to be a bit careful with operator
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
#' # Unquoting is especially useful for transforming a captured
#' # expression:
#' f <- ~foo(bar)
#' f <- quo(inner(!! f, arg1))
#' f <- quo(outer(!! f, !!! lapply(letters[1:3], as_symbol)))
#' f
#'
#' # Note that it's fine to unquote formulas as long as you evaluate
#' # with eval_tidy():
#' f <- ~letters
#' f <- quo(toupper(!! f))
#' eval_tidy(f)
#'
#' # Formulas carry scope information about the inner expression
#' # inlined in the outer expression upon unquoting. To see this,
#' # let's create a formula that quotes a symbol that only exists in a
#' # local scope (a child environment of the current environment):
#' f1 <- local({ foo <- "foo"; ~foo })
#'
#' # You can evaluate that expression with eval_tidy():
#' eval_tidy(f1)
#'
#' # And you can also inline it in another expression before
#' # evaluation:
#' f2 <- local({ bar <- "bar"; ~toupper(bar)})
#' f3 <- quo(paste(!!f1, !!f2, "!"))
#' f3
#'
#' # eval_tidy() treats one-sided formulas like promises to be evaluated:
#' eval_tidy(f3)
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
#' quo(nse_function(UQ(var)))
#' quo(nse_function(UQE(var)))
#'
#' # This is equivalent to unquoting and taking the RHS:
#' quo(nse_function(!! f_rhs(var)))
#'
#' # One of the most important old-style NSE function is the dollar
#' # operator. You need to use UQE() for subsetting with dollar:
#' var <- ~cyl
#' quo(mtcars$UQE(var))
#'
#' # `!!`() is also treated as a shortcut. It is meant for situations
#' # where the bang operator would not parse, such as subsetting with
#' # $. Since that's its main purpose, we've made it a shortcut for
#' # UQE() rather than UQ():
#' var <- ~cyl
#' quo(mtcars$`!!`(var))
#'
#'
#' # Sometimes you would like to unquote an object containing a
#' # formula but include it as is rather than treating it as a
#' # promise. You can use UQF() for this purpose:
#' var <- ~letters[1:2]
#' f <- quo(list(!!var, UQF(var)))
#' f
#' eval_tidy(f)
#'
#' # Note that two-sided formulas are never treated as fpromises:
#' eval_tidy(quo(a ~ b))
#' @useDynLib rlang rlang_interp
quo <- function(expr) {
  enquo(expr)
}

#' Untidy quotation of an expression.
#'
#' Unlike [quo()], `expr()` returns a raw expression instead of a
#' formula. As a result, `expr()` is untidy in the sense that it does
#' not preserve scope information for the quoted expression. It can
#' still be useful in certain cases.  Compared to base R's
#' [base::quote()], it unquotes the expression on capture, and
#' compared to [quo()], the quoted expression is directly compatible
#' with the base R [base::eval()] function.
#'
#' @inheritParams quosure
#' @seealso See [quo()] and [expr_interp()] for more
#'   explanation on tidy quotation.
#' @return The raw expression supplied as argument.
#' @export
#' @examples
#' # The advantage of expr() over quote() is that it unquotes on
#' # capture:
#' expr(list(1, !! 3 + 10))
#'
#' # Unquoting can be especially useful for successive transformation
#' # of a captured expression:
#' (expr <- quote(foo(bar)))
#' (expr <- expr(inner(!! expr, arg1)))
#' (expr <- expr(outer(!! expr, !!! lapply(letters[1:3], as.symbol))))
#'
#' # Unlike quo(), expr() produces expressions that can
#' # be evaluated with base::eval():
#' e <- quote(letters)
#' e <- expr(toupper(!!e))
#' eval(e)
#'
#' # Be careful if you unquote a formula-quote: you need to take the
#' # RHS (and lose the scope information) to evaluate with eval():
#' f <- ~letters
#' e <- expr(toupper(!! f_rhs(f)))
#' eval(e)
#'
#' # However it's fine to unquote formulas if you evaluate with eval_tidy():
#' f <- ~letters
#' e <- expr(toupper(!! f))
#' eval_tidy(e)
expr <- function(expr) {
  expr <- substitute(expr)
  .Call(rlang_interp, expr, parent.frame())
}

#' Tidy quotation of multiple expressions and dots.
#'
#' `dots_quos()` quotes its arguments and returns them as a list of
#' tidy quotes. It is especially useful to "capture" arguments
#' forwarded through `...`.
#'
#' Both `dots_quos` and `dots_definitions()` have specific support for
#' definition expressions of the type `var := expr`, with some
#' differences:
#'
#'\describe{
#'  \item{`dots_quos()`}{
#'    When `:=` definitions are supplied to `dots_quos()`,
#'    they are treated as a synonym of argument assignment
#'    `=`. On the other hand, they allow unquoting operators on
#'    the left-hand side, which makes it easy to assign names
#'    programmatically.}
#'  \item{`dots_definitions()`}{
#'    This dots capturing function returns definitions as is. Unquote
#'    operators are processed on capture, in both the LHS and the
#'    RHS. Unlike `dots_quos()`, it allows named definitions.}
#' }
#' @inheritParams enquo
#' @param .named Whether to ensure all dots are named. Unnamed
#'   elements are processed with [expr_text()] to figure out a default
#'   name. If an integer, it is passed to the `width` argument of
#'   `expr_text()`, if `TRUE`, the default width is used. See
#'   [exprs_auto_name()].
#' @return A list of quosures with class `quosures`.
#' @name quosures
#' @export
#' @examples
#' # dots_quos() is like the singular version but allows quoting
#' # several arguments:
#' dots_quos(foo(), bar(baz), letters[1:2], !! letters[1:2])
#'
#' # It is most useful when used with dots. This allows quoting
#' # expressions across different levels of function calls:
#' fn <- function(...) dots_quos(...)
#' fn(foo(bar), baz)
#'
#' # Note that dots_quos() does not check for duplicate named
#' # arguments:
#' fn <- function(...) dots_quos(x = x, ...)
#' fn(x = a + b)
#'
#'
#' # Dots can be spliced in:
#' args <- list(x = 1:3, y = ~var)
#' dots_quos(!!! args, z = 10L)
#'
#' # Raw expressions are turned to formulas:
#' args <- alist(x = foo, y = bar)
#' dots_quos(!!! args)
#'
#'
#' # Definitions are treated similarly to named arguments:
#' dots_quos(x := expr, y = expr)
#'
#' # However, the LHS of definitions can be unquoted. The return value
#' # must be a symbol or a string:
#' var <- "foo"
#' dots_quos(!!var := expr)
#'
#' # If you need the full LHS expression, use dots_definitions():
#' dots <- dots_definitions(var = foo(baz) := bar(baz))
#' dots$defs
dots_quos <- function(..., .named = FALSE) {
  dots <- dots_capture(...)
  dots <- dots_interp_lhs(dots)
  if (.named) {
    width <- quo_names_width(.named)
    dots <- exprs_auto_name(dots, width)
  }
  struct(dots, class = "quosures")
}
#' @rdname quosures
#' @export
quos <- dots_quos

#' @rdname quosures
#' @export
is_quosures <- function(x) {
  inherits(x, "quosures")
}
#' @export
`[.quosures` <- function(x, i) {
  struct(NextMethod(), class = "quosures")
}
#' @export
c.quosures <- function(..., recursive = FALSE) {
  structure(NextMethod(), class = "quosures")
}

quo_names_width <- function(named) {
  if (is_true(named)) {
    60L
  } else if (is_scalar_integerish(named)) {
    named
  } else {
    abort("`.named` must be a scalar logical or a numeric")
  }
}

#' @rdname quosures
#' @export
dots_definitions <- function(..., .named = FALSE) {
  dots <- dots_capture(...)
  if (.named) {
    width <- quo_names_width(.named)
    dots <- exprs_auto_name(dots, width)
  }

  defined <- map_lgl(dots, function(dot) is_definition(f_rhs(dot)))
  defs <- map(dots[defined], as_definition)

  list(dots = dots[!defined], defs = defs)
}

as_definition <- function(dot) {
  env <- f_env(dot)
  pat <- f_rhs(dot)

  lhs <- .Call(rlang_interp, f_lhs(pat), env)
  rhs <- .Call(rlang_interp, f_rhs(pat), env)

  list(
    lhs = new_quosure(lhs, env),
    rhs = new_quosure(rhs, env)
  )
}


#' Ensure that list of expressions are all named.
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or tidy
#' quotes). The expressions are deparsed with [expr_text()].
#'
#' @param exprs A list of expressions or expression wrappers,
#'   e.g. tidy quotes.
#' @param width Maximum width of names.
#' @export
exprs_auto_name <- function(exprs, width = 60L) {
  have_name <- have_name(exprs)

  if (any(!have_name)) {
    nms <- map_chr(exprs[!have_name], quo_text, width = width)
    names(exprs)[!have_name] <- nms
  }

  exprs
}
