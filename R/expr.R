#' Is an object an expression?
#'
#' @description
#' In rlang, an _expression_ is the return type of [parse_expr()], the
#' set of objects that can be obtained from parsing R code. Under this
#' definition expressions include numbers, strings, `NULL`, symbols,
#' and function calls. These objects can be classified as:
#'
#' * Symbolic objects, i.e. symbols and function calls (for which
#'   `is_symbolic()` returns `TRUE`)
#' * Syntactic literals, i.e. scalar atomic objects and `NULL`
#'   (testable with `is_syntactic_literal()`)
#'
#' `is_expression()` returns `TRUE` if the input is either a symbolic
#' object or a syntactic literal. If a call, the elements of the call
#' must all be expressions as well. Unparsable calls are not
#' considered expressions in this narrow definition.
#'
#' Note that in base R, there exists [expression()] vectors, a data
#' type similar to a list that supports special attributes created by
#' the parser called source references. This data type is not
#' supported in rlang.
#'
#' @details
#' `is_symbolic()` returns `TRUE` for symbols and calls (objects with
#' type `language`). Symbolic objects are replaced by their value
#' during evaluation. Literals are the complement of symbolic
#' objects. They are their own value and return themselves during
#' evaluation.
#'
#' `is_syntactic_literal()` is a predicate that returns `TRUE` for the
#' subset of literals that are created by R when parsing text (see
#' [parse_expr()]): numbers, strings and `NULL`. Along with symbols,
#' these literals are the terminating nodes in an AST.
#'
#' Note that in the most general sense, a literal is any R object that
#' evaluates to itself and that can be evaluated in the empty
#' environment. For instance, `quote(c(1, 2))` is not a literal, it is
#' a call. However, the result of evaluating it in [base_env()] is a
#' literal(in this case an atomic vector).
#'
#' As the data structure for function arguments, pairlists are also a
#' kind of language objects. However, since they are mostly an
#' internal data structure and can't be returned as is by the parser,
#' `is_expression()` returns `FALSE` for pairlists.
#'
#' @param x An object to test.
#' @seealso [is_call()] for a call predicate.
#' @export
#' @examples
#' q1 <- quote(1)
#' is_expression(q1)
#' is_syntactic_literal(q1)
#'
#' q2 <- quote(x)
#' is_expression(q2)
#' is_symbol(q2)
#'
#' q3 <- quote(x + 1)
#' is_expression(q3)
#' is_call(q3)
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
#' fmls <- formals(is_expression)
#' typeof(fmls)
#'
#' # Since they are mostly an internal data structure, is_expression()
#' # returns FALSE for pairlists, so you will have to check explicitly
#' # for them:
#' is_expression(fmls)
#' is_pairlist(fmls)
is_expression <- function(x) {
  stack <- new_stack()
  stack$push(x)

  while (!is_exhausted(elt <- stack$pop())) {
    if (is_missing(elt)) {
      return(FALSE)
    }

    switch(
      typeof(elt),
      language = stack$push(!!!as.list(elt)),
      if (!is_symbol(elt) && !is_syntactic_literal(elt)) {
        return(FALSE)
      }
    )
  }

  TRUE
}
#' @export
#' @rdname is_expression
is_syntactic_literal <- function(x) {
  switch(typeof(x),
    NULL = {
      TRUE
    },

    logical = ,
    integer = ,
    double = ,
    character = {
      length(x) == 1
    },

    complex = {
      if (length(x) != 1) {
        return(FALSE)
      }
      is_na(x) || Re(x) == 0
    },

    FALSE
  )
}
#' @export
#' @rdname is_expression
is_symbolic <- function(x) {
  typeof(x) %in% c("language", "symbol")
}


#' Turn an expression to a label
#'
#' @keywords internal
#' @description
#'
#' `r lifecycle::badge("questioning")`
#'
#' `expr_text()` turns the expression into a single string, which
#' might be multi-line. `expr_name()` is suitable for formatting
#' names. It works best with symbols and scalar types, but also
#' accepts calls. `expr_label()` formats the expression nicely for use
#' in messages.
#'
#' @param expr An expression to labellise.
#'
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
#' @export
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
  if (is_null(expr)) {
    return("NULL")
  }

  if (is_symbol(expr)) {
    return(as_string(expr))
  }

  if (is_call(expr)) {
    if (is_data_pronoun(expr)) {
      name <- data_pronoun_name(expr) %||% "<unknown>"
    } else {
      name <- deparse_one(expr)
      name <- gsub("\n.*$", "...", name)
    }
    return(name)
  }

  # So 1L is translated to "1" and not "1L"
  if (is_scalar_atomic(expr)) {
    return(as.character(expr))
  }

  if (length(expr) == 1) {
    name <- expr_text(expr)
    name <- gsub("\n.*$", "...", name)
    return(name)
  }

  abort(sprintf(
    "%s must be a symbol, scalar, or call.",
    format_arg("expr")
  ))
}
#' @rdname expr_label
#' @export
#' @param width Width of each line.
#' @param nlines Maximum number of lines to extract.
expr_text <- function(expr, width = 60L, nlines = Inf) {
  if (is_symbol(expr)) {
    return(sym_text(expr))
  }

  str <- deparse(expr, width.cutoff = width, backtick = TRUE)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}

sym_text <- function(sym) {
  # Use as_string() to translate unicode tags
  text <- as_string(sym)

  if (needs_backticks(text)) {
    text <- sprintf("`%s`", text)
  }

  text
}

deparse_one <- function(expr) {
  str <- deparse(expr, 60L)

  if (length(str) > 1) {
    if (is_call(expr, function_sym)) {
      expr[[3]] <- quote(...)
      str <- deparse(expr, 60L)
    } else if (is_call(expr, brace_sym)) {
      str <- "{ ... }"
    } else if (is_call(expr)) {
      str <- deparse(call2(expr[[1]], quote(...)), 60L)
    }
    str <- paste(str, collapse = "\n")
  }

  str
}

#' Set and get an expression
#'
#' @keywords internal
#' @description
#'
#' These helpers are useful to make your function work generically
#' with quosures and raw expressions. First call `get_expr()` to
#' extract an expression. Once you're done processing the expression,
#' call `set_expr()` on the original object to update the expression.
#' You can return the result of `set_expr()`, either a formula or an
#' expression depending on the input type. Note that `set_expr()` does
#' not change its input, it creates a new object.
#'
#' @param x An expression, closure, or one-sided formula. In addition,
#'   `set_expr()` accept frames.
#' @param value An updated expression.
#' @param default A default expression to return when `x` is not an
#'   expression wrapper. Defaults to `x` itself.
#' @return The updated original input for `set_expr()`. A raw
#'   expression for `get_expr()`.
#' @seealso [quo_get_expr()] and [quo_set_expr()] for versions of
#'   [get_expr()] and [set_expr()] that only work on quosures.
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
  if (is_quosure(x)) {
    x <- quo_set_expr(x, value)
  } else if (is_formula(x)) {
    f_rhs(x) <- value
  } else if (is_closure(x)) {
    body(x) <- value
  } else {
    x <- value
  }

  x
}
#' @rdname set_expr
#' @export
get_expr <- function(x, default = x) {
  .Call(ffi_get_expression, x, default)
}

expr_type_of <- function(x) {
  if (is_missing(x)) {
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


#' Print an expression
#'
#' @description
#'
#' `expr_print()`, powered by `expr_deparse()`, is an alternative
#' printer for R expressions with a few improvements over the base R
#' printer.
#'
#' * It colourises [quosures][nse-defuse] according to their environment.
#'   Quosures from the global environment are printed normally while
#'   quosures from local environments are printed in unique colour (or
#'   in italic when all colours are taken).
#'
#' * It wraps inlined objects in angular brackets. For instance, an
#'   integer vector unquoted in a function call (e.g.
#'   `expr(foo(!!(1:3)))`) is printed like this: `foo(<int: 1L, 2L,
#'   3L>)` while by default R prints the code to create that vector:
#'   `foo(1:3)` which is ambiguous.
#'
#' * It respects the width boundary (from the global option `width`)
#'   in more cases.
#'
#' @param x An object or expression to print.
#' @param width The width of the deparsed or printed expression.
#'   Defaults to the global option `width`.
#' @param ... Arguments passed to `expr_deparse()`.
#'
#' @return `expr_deparse()` returns a character vector of lines.
#'   `expr_print()` returns its input invisibly.
#'
#' @export
#' @examples
#' # It supports any object. Non-symbolic objects are always printed
#' # within angular brackets:
#' expr_print(1:3)
#' expr_print(function() NULL)
#'
#' # Contrast this to how the code to create these objects is printed:
#' expr_print(quote(1:3))
#' expr_print(quote(function() NULL))
#'
#' # The main cause of non-symbolic objects in expressions is
#' # quasiquotation:
#' expr_print(expr(foo(!!(1:3))))
#'
#'
#' # Quosures from the global environment are printed normally:
#' expr_print(quo(foo))
#' expr_print(quo(foo(!!quo(bar))))
#'
#' # Quosures from local environments are colourised according to
#' # their environments (if you have crayon installed):
#' local_quo <- local(quo(foo))
#' expr_print(local_quo)
#'
#' wrapper_quo <- local(quo(bar(!!local_quo, baz)))
#' expr_print(wrapper_quo)
expr_print <- function(x, ...) {
  cat_line(expr_deparse(x, ...))
  invisible(x)
}
#' @rdname expr_print
#' @export
expr_deparse <- function(x,
                         ...,
                         width = peek_option("width")) {
  check_dots_empty0(...)
  deparser <- new_quo_deparser(width = width)
  quo_deparse(x, deparser)
}
