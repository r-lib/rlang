#' Is an object a language object?
#'
#' These helpers are consistent wrappers around their base R
#' equivalents. \code{is_lang()} and its alias \code{is_language()}
#' test for language objects, the set of objects that can be obtained
#' from parsing R code. A language object can be one of two things:
#' either a symbolic object (for which \code{is_symbolic()} returns
#' \code{TRUE}), or a parsable literal (testable with
#' \code{is_parsable_literal()}).
#'
#' Literals are the set of R objects that evaluate to themselves when
#' supplied to \code{eval()}. Non-literal objects are called symbolic.
#' Only symbols and calls are symbolic. They form the backbone of an
#' expression, and their values are looked up by the R interpreter
#' when the expression is evaluated. By contrast, literals are
#' returned as is. Scalar strings, numbers, booleans, and the
#' \code{NULL} object form a special subset of literals. They are the
#' only non-symbolic objects that can arise from parsing a string or
#' an R source file (e.g. from using \code{\link{parse_expr}()}). You
#' can test for them with \code{is_parsable_literal()}.
#'
#' \code{is_parsable_literal()} is a predicate that returns
#' \code{TRUE} for the subset of literals that are created by R when
#' parsing text (see \code{\link{parse_expr}()}): numbers, strings and
#' \code{NULL}. Along with symbols, these literals are the
#' terminating nodes in a parse tree. Note that in the most general
#' sense, a literal is any R object that evaluates to itself and that
#' can be evaluated in the empty environment. For instance,
#' \code{quote(c(1, 2))} is not a literal, but the result of
#' evaluating it in \code{\link{base_env}()} is (in this case an
#' atomic vector).  Technically, this sort of literal objects can be
#' inlined in language expressions. If your function accepts arbitrary
#' expressions, it should thus account for that possibility with a
#' catch-all branch. On the other hand, if your function only gets
#' expressions created from a parse, \code{quote()}, or
#' \code{\link{tidy_capture}()}, then you can check for literals with
#' \code{is_parsable_literal()}.
#'
#' Finally, pairlists can also be language objects. However, since
#' they are mostly an internal data structure, \code{is_lang()}
#' returns \code{FALSE} for pairlists. You can use
#' \code{is_pairlist()} to explicitly check for them. Pairlists are
#' the data structure for function arguments. They usually do not
#' arise from R code because subsetting a call is a type-preserving
#' operation. However, you can obtain the pairlist of arguments by
#' taking the CDR of the call object from C code. The rlang function
#' \code{\link{call_args_lsp}()} will do it from R. Another way in
#' which pairlist of arguments arise is by extracting the argument
#' list of a closure with \code{\link[base]{formals}()} or
#' \code{\link{fn_fmls}()}.
#'
#' @param x An object to test. When you supply a tidy quote (see
#'   \code{\link{tidy_quote}()}) to any of the language predicates,
#'   they will perform their test on the RHS of the formula.
#' @seealso \code{\link{is_call}()} for a call predicate.
#'   \code{\link{as_symbol}()} and \code{\link{as_call}()} for coercion
#'   functions.
#' @export
#' @examples
#' q1 <- quote(1)
#' is_lang(q1)
#' is_parsable_literal(q1)
#'
#' q2 <- quote(x)
#' is_lang(q2)
#' is_symbol(q2)
#'
#' q3 <- quote(x + 1)
#' is_lang(q3)
#' is_call(q3)
#'
#'
#' # Since tidy quotes are an important way of representing
#' # expressions in R, all language predicates will test the RHS of
#' # the formula if you supply one:
#' is_symbol(~foo)
#' is_call(~foo)
#' is_symbol(~foo(bar))
#' is_call(~foo(bar))
#'
#'
#' # Atomic language objects are the terminating nodes of a call
#' # tree: NULL or a scalar atomic vector:
#' is_parsable_literal("string")
#' is_parsable_literal(NULL)
#'
#' is_parsable_literal(letters)
#' is_parsable_literal(quote(call()))
#'
#' # Parsable literals have the property of being self-quoting:
#' identical("foo", quote("foo"))
#' identical(1L, quote(1L))
#' identical(NULL, quote(NULL))
#'
#' # Like any literals, they can be evaluated within the empty
#' # environment:
#' eval(quote(1L), empty_env())
#'
#' # Whereas it would fail for symbolic language objects:
#' # eval(quote(c(1L, 2L)), empty_env())
#'
#'
#' # Pairlists are also language objects representing argument lists.
#' # You will usually encounter them with extracted formals:
#' fmls <- formals(is_lang)
#' typeof(fmls)
#'
#' # Since they are mostly an internal data structure, is_lang()
#' # returns FALSE for pairlists, so you will have to check explicitly
#' # for them:
#' is_lang(fmls)
#' is_pairlist(fmls)
#'
#' # Note that you can also extract call arguments as a pairlist:
#' call_args_lsp(quote(fn(arg1, arg2 = "foo")))
is_lang <- function(x) {
  x <- expr(x)
  is_symbolic(x) || is_parsable_literal(x)
}
#' @rdname is_lang
#' @export
is_language <- is_lang
#' @rdname is_lang
#' @export
is_symbol <- function(x) {
  x <- expr(x)
  typeof(x) == "symbol"
}
#' @export
#' @rdname is_lang
is_parsable_literal <- function(x) {
  x <- expr(x)
  typeof(x) == "NULL" || (length(x) == 1 && typeof(x) %in% parsable_atomic_types)
}
#' @export
#' @rdname is_lang
is_symbolic <- function(x) {
  x <- expr(x)
  typeof(x) %in% c("language", "symbol")
}

#' @rdname is_lang
#' @export
is_pairlist <- function(x) {
  typeof(x) == "pairlist"
}

#' Is object a call?
#'
#' This function tests if \code{x} is a call. This is a
#' pattern-matching predicate that will return \code{FALSE} if
#' \code{name} and \code{n} are supplied and the call does not match
#' these properties. \code{is_unary_call()} and
#' \code{is_binary_call()} hardcode \code{n} to 1 and 2.
#'
#' @param x An object to test. If a formula, the right-hand side is
#'   extracted.
#' @param name An optional name that the call should match. It is
#'   passed to \code{\link{as_symbol}()} before matching. This argument
#'   is vectorised and you can supply a vector of names to match. In
#'   this case, \code{is_call()} returns \code{TRUE} if at least one
#'   name matches.
#' @param n An optional number of arguments that the call should
#'   match.
#' @seealso \code{\link{is_lang}()}
#' @export
#' @examples
#' is_call(quote(foo(bar)))
#'
#' # Right-hand sides are extracted from formulas:
#' is_call(~foo(bar))
#'
#' # You can pattern-match the call with additional arguments:
#' is_call(~foo(bar), "foo")
#' is_call(~foo(bar), "bar")
#' is_call(~foo(bar), quote(foo))
#'
#' # Match the number of arguments with is_call():
#' is_call(~foo(bar), "foo", 1)
#' is_call(~foo(bar), "foo", 2)
#'
#' # Or more specifically:
#' is_unary_call(~foo(bar))
#' is_unary_call(~ +3)
#' is_unary_call(~ 1 + 3)
#' is_binary_call(~ 1 + 3)
#'
#' # Namespaced calls are a bit tricky. Strings won't work because
#' # as_symbol("base::list") returns a symbol rather than a namespace
#' # call:
#' is_call(~base::list(baz), "base::list")
#'
#' # However you can use the fact that as_symbol(quote(base::list()))
#' # extracts the function identifier as is, and thus returns the call
#' # base::list:
#' is_call(~base::list(baz), ~base::list(), 1)
#'
#'
#' # The name argument is vectorised so you can supply a list of names
#' # to match with:
#' is_call(~foo(bar), c("bar", "baz"))
#' is_call(~foo(bar), c("bar", "foo"))
#' is_call(~base::list, c("::", ":::", "$", "@"))
is_call <- function(x, name = NULL, n = NULL) {
  x <- expr(x)

  if (typeof(x) != "language") {
    return(FALSE)
  }

  if (!is_null(name)) {
    # Wrap language objects in a list
    if (!is_vector(name)) {
      name <- list(name)
    }

    unmatched <- TRUE
    for (elt in name) {
      if (identical(x[[1]], as_symbol(elt))) {
        unmatched <- FALSE
        break
      }
    }

    if (unmatched) {
      return(FALSE)
    }
  }

  if (!is_null(n) && !has_length(x, n + 1L)) {
    return(FALSE)
  }

  TRUE
}

#' @rdname is_call
#' @export
is_unary_call <- function(x, name = NULL) {
  is_call(x, name, n = 1L)
}
#' @rdname is_call
#' @export
is_binary_call <- function(x, name = NULL) {
  is_call(x, name, n = 2L)
}


#' Coerce an object to a name or call.
#'
#' These coercing functions can transform names, calls, formulas, and
#' strings. The distinction between a name and a call is particularly
#' important when coercing from a string. Coercing to a call will
#' parse the string, coercing to a name will create a (potentially)
#' non-syntactic name.
#'
#' @param x An object to coerce
#' @export
#' @return \code{as_symbol()} and \code{as_call()} return a symbol or
#'   a call. \code{as_name()} returns a string.
#' @examples
#' as_symbol("x + y")
#' as_call("x + y")
#'
#' as_call(~ f)
#' as_symbol(~ f())
as_symbol <- function(x) {
  switch(typeof(x),
    symbol = x,
    character =
      if (length(x) == 1) {
        symbol(x)
      } else {
        abort("Cannot parse character vector of length > 1")
      },
    language =
      if (is_formula(x)) {
        as_symbol(f_rhs(x))
      } else if (is_prefixed_name(x)) {
        x
      } else {
        as_symbol(x[[1]])
      },
    abort_switchpatch(x)
  )
}
#' @export
#' @rdname as_symbol
as_name <- function(x) {
  if (is_string(x)) {
    x
  } else {
    as_string(as_symbol(x))
  }
}

#' @export
#' @rdname as_symbol
as_call <- function(x) {
  UseMethod("as_call")
}
#' @export
as_call.name <- function(x) {
  new_call(x)
}
#' @export
as_call.call <- function(x) {
  x
}
#' @export
as_call.character <- function(x) {
  if (!is_string(x)) {
    abort("Cannot parse character vector of length > 1")
  }
  parse_expr(x)
}
#' @export
as_call.formula <- function(x) {
  as_call(f_rhs(x))
}

is_prefixed_name <- function(x) {
  fn <- x[[1]]
  if (is_symbol(fn)) {
    as_character(fn) %in% c("::", ":::", "$", "@")
  } else {
    FALSE
  }
}

expr <- function(x) {
  if (is_tidy_quote_(x)) f_rhs(x) else x
}
