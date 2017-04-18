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
#' `quo()` interpolates on capture (see [enquo()] and
#' `vignette("tidy-eval")`). Alternatively, `expr_interp()` allows you
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
#' @aliases UQ UQE UQS
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
#' f <- quo(foo(bar))
#' f <- quo(inner(!! f, arg1))
#' f <- quo(outer(!! f, !!! lapply(letters[1:3], as_symbol)))
#' f
#'
#' # Note that it's fine to unquote expressions to be evaluated as
#' # quosures, as long as you evaluate with eval_tidy():
#' f <- quo(letters)
#' f <- quo(toupper(!! f))
#' eval_tidy(f)
#'
#' # Quosures carry scope information about the inner expression
#' # inlined in the outer expression upon unquoting. To see this,
#' # let's create a quosure that quotes a symbol that only exists in a
#' # local scope (a child environment of the current environment):
#' f1 <- locally({ foo <- "foo"; quo(foo) })
#'
#' # You can evaluate that expression with eval_tidy():
#' eval_tidy(f1)
#'
#' # And you can also inline it in another expression before
#' # evaluation:
#' f2 <- locally({ bar <- "bar"; quo(toupper(bar))})
#' f3 <- quo(paste(!!f1, !!f2, "!"))
#' f3
#'
#' # eval_tidy() treats quosures as promises to be evaluated:
#' eval_tidy(f3)
#'
#'
#' # Quoting as a quosure is necessary to preserve scope information
#' # and make sure objects are looked up in the right place. However,
#' # there are situations where it can get in the way. This is the
#' # case when you deal with non-tidy NSE functions that do not
#' # understand formulas. You can inline the RHS of a formula in a
#' # call thanks to the UQE() operator:
#' nse_function <- function(arg) substitute(arg)
#' var <- locally(quo(foo(bar)))
#' quo(nse_function(UQ(var)))
#' quo(nse_function(UQE(var)))
#'
#' # This is equivalent to unquoting and taking the RHS:
#' quo(nse_function(!! get_expr(var)))
#'
#' # One of the most important old-style NSE function is the dollar
#' # operator. You need to use UQE() for subsetting with dollar:
#' var <- quo(cyl)
#' quo(mtcars$UQE(var))
#'
#' # `!!`() is also treated as a shortcut. It is meant for situations
#' # where the bang operator would not parse, such as subsetting with
#' # $. Since that's its main purpose, we've made it a shortcut for
#' # UQE() rather than UQ():
#' var <- quo(cyl)
#' quo(mtcars$`!!`(var))
#'
#'
#' # Quosures are represented as formulas, but formulas are not
#' # treated identically to quosures. Formulas are guarded on capture
#' # so that they evaluate to a literal formula rather than
#' # self-evaluate like quosures:
#' quo_f <- ~letters[1:2]
#' quo <- quo(letters[1:2])
#'
#' f <- quo(list(!!quo_f, !! quo))
#' f
#' eval_tidy(f)
#'
#'
#' # When a quosure is printed in the console, the brackets indicate
#' # if the enclosure is the global environment or a local one:
#' locally(quo(foo))
#'
#' # Literals are enquosed with the empty environment because they can
#' # be evaluated anywhere:
#' quo(10L)
#'
#' # To differentiate local environments, use str(). It prints the
#' # machine address of the environment:
#' quo1 <- locally(quo(foo))
#' quo2 <- locally(quo(foo))
#' quo1; quo2
#' str(quo1); str(quo2)
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
#' # Be careful if you unquote a quosure: you need to take the RHS
#' # (and lose the scope information) to evaluate with eval():
#' f <- quo(letters)
#' e <- expr(toupper(!! get_expr(f)))
#' eval(e)
#'
#' # On the other hand it's fine to unquote quosures if you evaluate
#' # with eval_tidy():
#' f <- quo(letters)
#' e <- expr(toupper(!! f))
#' eval_tidy(e)
expr <- function(expr) {
  enexpr(expr)
}

#' Is an object a quosure or quosure-like?
#'
#' @description
#'
#' These predicates test for [quosure][new_quosure] objects.
#'
#' - `is_quosure()` tests for the canonical R quosure: the one-sided
#'   "formula".
#'
#' - `is_quosureish()` tests for general R quosure objects: quosures,
#'   two-sided formulas, and [definitions][op-definition].
#'
#'
#' @details
#'
#' The `scoped` argument patterns-match on whether the scoped bundled
#' with the quosure is valid or not. Invalid scopes may happen in
#' nested quotations like `~~expr`, where the outer quosure is validly
#' scoped but not the inner one. This is because `~` saves the
#' environment when it is evaluated, and quoted quosures are by
#' definition not evaluated. Note that in the [tidy evaluation
#' framework][eval_tidy], unscoped quosures are automatically given
#' the environment of the outer quosure during the evaluation process.
#'
#' @param x An object to test.
#' @param scoped A boolean indicating whether the quosure or formula
#'   is scoped, that is, has a valid environment attribute. If `NULL`,
#'   the scope is not inspected.
#' @seealso [as_quosure()][new_quosure] and [new_quosure()] for creating
#'   quosures, and [quo()] or [eval_tidy()] for information
#'   about the role of quosures in the tidy evaluation framework.
#' @export
#' @examples
#' # Degenerate quosures are often created by quoting, since `~`
#' # records the environment when it is evaluated the first time:
#' f <- ~~expr
#'
#' # The outer quosure has been evaluated and is scoped:
#' is_quosure(f, scoped = TRUE)
#'
#' # But the inner formula is not:
#' inner_f <- f_rhs(f)
#' is_quosure(inner_f, scoped = TRUE)
#'
#'
#' # Formulas not quosures:
#' is_quosure(~foo)
#'
#' # But they are quosureish:
#' is_quosureish(~foo)
#'
#' # Though two-sided formulas are not quosureish:
#' is_quosureish(a ~ b)
is_quosure <- function(x, scoped = NULL) {
  if (!inherits(x, "quosure")) {
    return(FALSE)
  }
  if (!is_null(scoped) && scoped != is_env(f_env(x))) {
    return(FALSE)
  }
  TRUE
}
#' @rdname is_quosure
#' @export
is_quosureish <- function(x, scoped = NULL) {
  is_formula(x, scoped = scoped, lhs = FALSE)
}
is_one_sided <- function(x, lang_sym = sym_tilde) {
  typeof(x) == "language" &&
    identical(node_car(x), lang_sym) &&
    is_null(node_cadr(node_cdr(x)))
}

#' Is a quosure empty?
#'
#' When missing arguments are captured as quosures, either through
#' [enquo()] or [quos()], they are returned as an empty quosure. These
#' quosures contain the [missing argument][missing_arg] and typically
#' have the [empty environment][empty_env] as enclosure.
#'
#' @param quo A quosure.
#' @export
#' @examples
#' quo <- quo()
#' quo_is_missing(quo)
#' is_missing(f_rhs(quo))
#' @rdname empty_quosure
#' @export
quo_is_missing <- function(quo) {
  is_quosure(quo) && is_missing(f_rhs(quo))
}

#' Create quosures.
#'
#' @description
#'
#' Quosure objects wrap an [expression][is_expr] with a [lexical
#' enclosure][env]. This is a powerful quoting (see [base::quote()]
#' and [quo()]) mechanism that makes it possible to carry and
#' manipulate expressions while making sure that its symbolic content
#' (symbols and named calls, see [is_symbolic()]) is correctly looked
#' up during evaluation.
#'
#' - `new_quosure()` creates a quosure from a raw expression and an
#'   environment.
#'
#' - `as_quosure()` is useful for functions that expect quosures but
#'   allow specifying a raw expression as well. It has two possible
#'   effects: if `x` is not a quosure, it wraps it into a quosure
#'   bundling `env` as scope. If `x` is an unscoped quosure (see
#'   [is_quosure()]), `env` is used as a default scope. On the other
#'   hand if `x` has a valid enclosure, it is returned as is (even if
#'   `env` is not the same as the formula environment).
#'
#' - While `as_quosure()` always returns a quosure (a one-sided
#'   formula), even when its input is a [formula][new_formula] or a
#'   [definition][op-definition], `as_quosureish()` returns quosureish
#'   inputs as is.
#'
#' @inheritParams new_formula
#' @param x An object to convert.
#' @param env An environment specifying the lexical enclosure of the
#'   quosure.
#' @seealso [is_quosure()]
#' @export
#' @examples
#' f <- new_quosure(quote(mtcars), as_env("datasets"))
#' f
#' eval_tidy(f)
#'
#'
#' # Sometimes you get unscoped formulas because of quotation:
#' f <- ~~expr
#' inner_f <- f_rhs(f)
#' str(inner_f)
#' is_quosureish(inner_f, scoped = TRUE)
#'
#' # You can use as_quosure() to provide a default environment:
#' as_quosure(inner_f, base_env())
#'
#' # Or convert expressions or any R object to a validly scoped quosure:
#' as_quosure(quote(expr), base_env())
#' as_quosure(10L, base_env())
#'
#'
#' # While as_quosure() always returns a quosure (one-sided formula),
#' # as_quosureish() returns quosureish objects:
#' as_quosure(a := b)
#' as_quosureish(a := b)
#' as_quosureish(10L)
new_quosure <- function(rhs, env = caller_env()) {
  quo <- new_formula(NULL, rhs, env)
  set_attrs(quo, class = c("quosure", "formula"))
}
#' @rdname new_quosure
#' @export
as_quosure <- function(x, env = caller_env()) {
  if (is_quosure(x)) {
    x
  } else {
    new_quosure(get_expr(x), quo_env(x, env))
  }
}
quo_expr <- function(quo, default = quo) {
  if (is_quosureish(quo)) {
    f_rhs(quo)
  } else {
    default
  }
}
quo_env <- function(quo, default) {
  if (is_quosureish(quo)) {
    f_env(quo) %||% default
  } else {
    default
  }
}

#' @export
print.quosure <- function(x, ...) {
  cat(paste0("<quosure: ", env_type(get_env(x)), ">\n"))
  print(zap_attrs(x))
  invisible(x)
}
#' @export
str.quosure <- function(object, ...) {
  env_type <- env_format(get_env(object))

  cat(paste0("<quosure: ", env_type, ">\n"))
  print(zap_attrs(object))
  invisible(object)
}

#' @rdname new_quosure
#' @export
as_quosureish <- function(x, env = caller_env()) {
  if (is_quosureish(x)) {
    if (!is_env(f_env(x))) {
      f_env(x) <- env
    }
    x
  } else if (is_frame(x)) {
    new_quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    new_quosure(get_expr(x), get_env(x, env))
  }
}

#' Splice a quosure and format it into string or label.
#'
#' `quo_expr()` flattens all quosures within an expression. I.e., it
#' turns `~foo(~bar(), ~baz)` to `foo(bar(), baz)`. `quo_text()` and
#' `quo_label()` are equivalent to [f_text()], [expr_label()], etc,
#' but they first splice their argument using `quo_expr()`.
#' `quo_name()` transforms a quoted symbol to a string. It adds a bit
#' more intent and type checking than simply calling `quo_text()` on
#' the quoted symbol (which will work but won't return an error if not
#' a symbol).
#'
#' @inheritParams expr_label
#' @param quo A quosure or expression.
#' @export
#' @seealso [expr_label()], [f_label()]
#' @examples
#' quo_expr(~foo(~bar))
#' quo_text(~foo(~bar))
#'
#' quo_name(~sym)
quo_expr <- function(quo) {
  quo_splice(duplicate(quo))
}
#' @rdname quo_expr
#' @export
quo_label <- function(quo) {
  expr_label(quo_expr(quo))
}
#' @rdname quo_expr
#' @export
quo_text <- function(quo, width = 60L, nlines = Inf) {
  expr_text(quo_expr(quo), width = width, nlines = nlines)
}
#' @rdname quo_expr
#' @export
quo_name <- function(quo) {
  expr_name(quo_expr(quo))
}

quo_splice <- function(x, parent = NULL) {
  switch_expr(x,
    language = {
      if (is_quosure(x)) {
        while (is_quosure(x)) {
          x <- f_rhs(x)
        }
        if (!is_null(parent)) {
          mut_node_car(parent, x)
        }
        quo_splice(x, parent)
      } else {
        quo_splice(node_cdr(x))
      }
    },
    pairlist = {
      while(!is_null(x)) {
        quo_splice(node_car(x), x)
        x <- node_cdr(x)
      }
    }
  )

  x
}
