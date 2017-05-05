#' Raw quotation of an expression
#'
#' @description
#'
#' These functions return raw expressions (whereas [quo()] and
#' variants return quosures). They support [quasiquotation]
#' syntax.
#'
#' - `expr()` returns its argument unevaluated. It is equivalent to
#'   [base::bquote()].
#'
#' - `enexpr()` takes an argument name and returns it unevaluated. It
#'   is equivalent to [base::substitute()].
#'
#' - `exprs()` captures multiple expressions and returns a list. In
#'   particular, it can capture expressions in `...`. It supports name
#'   unquoting with `:=` (see [quos()]). It is equivalent to
#'   `eval(substitute(alist(...)))`.
#'
#' See [is_expr()] for more about R expressions.
#'
#' @inheritParams quosure
#' @seealso [quo()], [is_expr()]
#' @return The raw expression supplied as argument. `exprs()` returns
#'   a list of expressions.
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
#'
#' # exprs() lets you unquote names with the definition operator:
#' nm <- "foo"
#' exprs(a = 1, !! nm := 2)
expr <- function(expr) {
  enexpr(expr)
}
#' @rdname expr
#' @export
enexpr <- function(arg) {
  if (missing(arg)) {
    return(missing_arg())
  }

  capture <- lang(captureArg, substitute(arg))
  arg <- eval_bare(capture, caller_env())
  .Call(rlang_interp, arg$expr, arg$env, TRUE)
}
#' @rdname expr
#' @inheritParams dots_values
#' @param ... Arguments to extract.
#' @export
exprs <- function(..., .ignore_empty = "trailing") {
  map(quos(..., .ignore_empty = .ignore_empty), f_rhs)
}


#' Is an object an expression?
#'
#' @description
#'
#' `is_expr()` tests for expressions, the set of objects that can be
#' obtained from parsing R code. An expression can be one of two
#' things: either a symbolic object (for which `is_symbolic()` returns
#' `TRUE`), or a syntactic literal (testable with
#' `is_syntactic_literal()`). Technically, calls can contain any R
#' object, not necessarily symbolic objects or syntactic
#' literals. However, this only happens in artificial
#' situations. Expressions as we define them only contain numbers,
#' strings, `NULL`, symbols, and calls: this is the complete set of R
#' objects that can be created when R parses source code (e.g. from
#' using [parse_expr()]).
#'
#' Note that we are using the term expression in its colloquial sense
#' and not to refer to [expression()] vectors, a data type that wraps
#' expressions in a vector and which isn't used much in modern R code.
#'
#' @details
#'
#' `is_symbolic()` returns `TRUE` for symbols and calls (objects with
#' type `language`). Symbolic objects are replaced by their value
#' during evaluation. Literals are the complement of symbolic
#' objects. They are their own value and return themselves during
#' evaluation.
#'
#' `is_syntactic_literal()` is a predicate that returns `TRUE` for the
#' subset of literals that are created by R when parsing text (see
#' [parse_expr()]): numbers, strings and `NULL`. Along with symbols,
#' these literals are the terminating nodes in a parse tree.
#'
#' Note that in the most general sense, a literal is any R object that
#' evaluates to itself and that can be evaluated in the empty
#' environment. For instance, `quote(c(1, 2))` is not a literal, it is
#' a call. However, the result of evaluating it in [base_env()] is a
#' literal(in this case an atomic vector).
#'
#' Pairlists are also a kind of language objects. However, since they
#' are mostly an internal data structure, `is_expr()` returns `FALSE`
#' for pairlists. You can use `is_pairlist()` to explicitly check for
#' them. Pairlists are the data structure for function arguments. They
#' usually do not arise from R code because subsetting a call is a
#' type-preserving operation. However, you can obtain the pairlist of
#' arguments by taking the CDR of the call object from C code. The
#' rlang function [lang_tail()] will do it from R. Another way in
#' which pairlist of arguments arise is by extracting the argument
#' list of a closure with [base::formals()] or [fn_fmls()].
#'
#' @param x An object to test.
#' @seealso [is_lang()] for a call predicate.
#' @export
#' @examples
#' q1 <- quote(1)
#' is_expr(q1)
#' is_syntactic_literal(q1)
#'
#' q2 <- quote(x)
#' is_expr(q2)
#' is_symbol(q2)
#'
#' q3 <- quote(x + 1)
#' is_expr(q3)
#' is_lang(q3)
#'
#'
#' # Atomic expressions are the terminating nodes of a call tree:
#' # NULL or a scalar atomic vector:
#' is_syntactic_literal("string")
#' is_syntactic_literal(NULL)
#'
#' is_syntactic_literal(letters)
#' is_syntactic_literal(quote(call()))
#'
#' # Parsable literals have the property of being self-quoting:
#' identical("foo", quote("foo"))
#' identical(1L, quote(1L))
#' identical(NULL, quote(NULL))
#'
#' # Like any literals, they can be evaluated within the empty
#' # environment:
#' eval_bare(quote(1L), empty_env())
#'
#' # Whereas it would fail for symbolic expressions:
#' # eval_bare(quote(c(1L, 2L)), empty_env())
#'
#'
#' # Pairlists are also language objects representing argument lists.
#' # You will usually encounter them with extracted formals:
#' fmls <- formals(is_expr)
#' typeof(fmls)
#'
#' # Since they are mostly an internal data structure, is_expr()
#' # returns FALSE for pairlists, so you will have to check explicitly
#' # for them:
#' is_expr(fmls)
#' is_pairlist(fmls)
#'
#' # Note that you can also extract call arguments as a pairlist:
#' lang_tail(quote(fn(arg1, arg2 = "foo")))
is_expr <- function(x) {
  is_symbolic(x) || is_syntactic_literal(x)
}
#' @export
#' @rdname is_expr
is_syntactic_literal <- function(x) {
  typeof(x) == "NULL" || (length(x) == 1 && typeof(x) %in% parsable_atomic_types)
}
#' @export
#' @rdname is_expr
is_symbolic <- function(x) {
  typeof(x) %in% c("language", "symbol")
}


#' Turn an expression to a label
#'
#' `expr_text()` turns the expression into a single string, which
#' might be multi-line. `expr_name()` is suitable for formatting
#' names. It works best with symbols and scalar types, but also
#' accepts calls. `expr_label()` formats the expression nicely for use
#' in messages.
#'
#' @param expr An expression to labellise.
#' @export
#' @examples
#' # To labellise a function argument, first capture it with
#' # substitute():
#' fn <- function(x) expr_label(substitute(x))
#' fn(x:y)
#'
#' # Strings are encoded
#' expr_label("a\nb")
#'
#' # Names and expressions are quoted with ``
#' expr_label(quote(x))
#' expr_label(quote(a + b + c))
#'
#' # Long expressions are collapsed
#' expr_label(quote(foo({
#'   1 + 2
#'   print(x)
#' })))
expr_label <- function(expr) {
  if (is.character(expr)) {
    encodeString(expr, quote = '"')
  } else if (is.atomic(expr)) {
    format(expr)
  } else if (is.name(expr)) {
    paste0("`", as.character(expr), "`")
  } else {
    chr <- deparse_one(expr)
    paste0("`", chr, "`")
  }
}
#' @rdname expr_label
#' @export
expr_name <- function(expr) {
  switch_type(expr,
    symbol = as_string(expr),
    quosure = ,
    language = {
      name <- deparse_one(expr)
      name <- gsub("\n.*$", "...", name)
      name
    },
    if (is_scalar_atomic(expr)) {
      # So 1L is translated to "1" and not "1L"
      as.character(expr)
    } else if (length(expr) == 1) {
      name <- expr_text(expr)
      name <- gsub("\n.*$", "...", name)
      name
    } else {
      abort("`expr` must quote a symbol, scalar, or call")
    }
  )
}
#' @rdname expr_label
#' @export
#' @param width Width of each line.
#' @param nlines Maximum number of lines to extract.
expr_text <- function(expr, width = 60L, nlines = Inf) {
  str <- deparse(expr, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}
deparse_one <- function(expr) {
  chr <- deparse(expr)
  if (length(chr) > 1) {
    dot_call <- lang(expr[[1]], quote(...))
    chr <- paste(deparse(dot_call), collapse = "\n")
  }
  chr
}

#' Set and get an expression
#'
#' These helpers are useful to make your function work generically
#' with quosures and raw expressions. First call `get_expr()` to
#' extract an expression. Once you're done processing the expression,
#' call `set_expr()` on the original object to update the expression.
#' You can return the result of `set_expr()`, either a formula or an
#' expression depending on the input type. Note that `set_expr()` does
#' not change its input, it creates a new object.
#'
#' @param x An expression or one-sided formula. In addition,
#'   `set_expr()` accept frames.
#' @param value An updated expression.
#' @param default A default expression to return when `x` is not an
#'   expression wrapper. Defaults to `x` itself.
#' @return The updated original input for `set_expr()`. A raw
#'   expression for `get_expr()`.
#' @export
#' @examples
#' f <- ~foo(bar)
#' e <- quote(foo(bar))
#' frame <- identity(identity(ctxt_frame()))
#'
#' get_expr(f)
#' get_expr(e)
#' get_expr(frame)
#'
#' set_expr(f, quote(baz))
#' set_expr(e, quote(baz))
set_expr <- function(x, value) {
  if (is_quosureish(x)) {
    f_rhs(x) <- value
    x
  } else {
    value
  }
}
#' @rdname set_expr
#' @export
get_expr <- function(x, default = x) {
  if (is_quosureish(x)) {
    f_rhs(x)
  } else if (inherits(x, "frame")) {
    x$expr
  } else {
    default
  }
}

expr_type_of <- function(x) {
  if (missing(x)) {
    return("missing")
  }

  type <- typeof(x)
  if (type %in% c("symbol", "language", "pairlist", "NULL")) {
    type
  } else {
    "literal"
  }
}
switch_expr <- function(.x, ...) {
  switch(expr_type_of(.x), ...)
}
