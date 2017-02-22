#' Is an object an expression?
#'
#' @description
#'
#' These helpers are consistent wrappers around their base R
#' equivalents. \code{is_expr()} tests for expressions, the set of
#' objects that can be obtained from parsing R code. An expression can
#' be one of two things: either a symbolic object (for which
#' \code{is_symbolic()} returns \code{TRUE}), or a parsable literal
#' (testable with \code{is_parsable_literal()}). Note that we are
#' using the term expression in its colloquial sense and not to refer
#' to \code{\link{expression}()} vectors, a data type that wraps
#' expressions in a vector and which has not much use in R.
#'
#' Technically, a call can contain any R object, not necessarily
#' language objects. However, this only happens in artificial
#' situations. Expressions as we define them only contain numbers,
#' strings, \code{NULL}, symbols, and calls: this is the complete set
#' of R objects that are created when R parses source code (e.g. from
#' using \code{\link{parse_expr}()}). These objects can be classified
#' as literals and symbolic objects. Symbolic objects like symbols and
#' calls are treated specially when R evaluates an expression. When a
#' symbol is evaluated, it is looked up and replaced by its
#' value. When a call is evaluated, its arguments are recursively
#' evaluated, and the corresponding function is called, and the call
#' is replaced by the returned value. On the other hand, literal
#' objects, such as numbers and strings, just return their own
#' value. To sum up, an expression can either be symbolic or a
#' parsable literal.
#'
#' @details
#'
#' \code{is_symbolic()} returns \code{TRUE} for symbols and calls
#' (objects with type \code{language}). Literals are the complement of
#' symbolic objects. \code{is_parsable_literal()} is a predicate that
#' returns \code{TRUE} for the subset of literals that are created by
#' R when parsing text (see \code{\link{parse_expr}()}): numbers,
#' strings and \code{NULL}. Along with symbols, these literals are the
#' terminating nodes in a parse tree.
#'
#' Note that in the most general sense, a literal is any R object that
#' evaluates to itself and that can be evaluated in the empty
#' environment. For instance, \code{quote(c(1, 2))} is not a literal,
#' it is a call.  However, the result of evaluating it in
#' \code{\link{base_env}()} is a literal(in this case an atomic vector).
#'
#' Pairlists are also a kind of language objects. However, since they
#' are mostly an internal data structure, \code{is_expr()} returns
#' \code{FALSE} for pairlists. You can use \code{is_pairlist()} to
#' explicitly check for them. Pairlists are the data structure for
#' function arguments. They usually do not arise from R code because
#' subsetting a call is a type-preserving operation. However, you can
#' obtain the pairlist of arguments by taking the CDR of the call
#' object from C code. The rlang function
#' \code{\link{call_args_lsp}()} will do it from R. Another way in
#' which pairlist of arguments arise is by extracting the argument
#' list of a closure with \code{\link[base]{formals}()} or
#' \code{\link{fn_fmls}()}.
#'
#' @param x An object to test. When you supply a tidy quote (see
#'   \code{\link{tidy_quote}()}) to any of the expression predicates,
#'   they will perform their test on the RHS of the formula.
#' @seealso \code{\link{is_lang}()} for a call predicate.
#'   \code{\link{as_symbol}()} and \code{\link{as_call}()} for coercion
#'   functions.
#' @export
#' @examples
#' q1 <- quote(1)
#' is_expr(q1)
#' is_parsable_literal(q1)
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
#' # Since tidy quotes are an important way of representing
#' # expressions in R, all expression predicates will test the RHS of
#' # the formula if you supply one:
#' is_symbol(~foo)
#' is_lang(~foo)
#' is_symbol(~foo(bar))
#' is_lang(~foo(bar))
#'
#'
#' # Atomic expressions are the terminating nodes of a call tree:
#' # NULL or a scalar atomic vector:
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
#' # Whereas it would fail for symbolic expressions:
#' # eval(quote(c(1L, 2L)), empty_env())
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
#' lang_args_lsp(quote(fn(arg1, arg2 = "foo")))
is_expr <- function(x) {
  x <- get_expr(x)
  is_symbolic(x) || is_parsable_literal(x)
}
#' @rdname is_expr
#' @export
is_symbol <- function(x) {
  x <- get_expr(x)
  typeof(x) == "symbol"
}
#' @export
#' @rdname is_expr
is_parsable_literal <- function(x) {
  x <- get_expr(x)
  typeof(x) == "NULL" || (length(x) == 1 && typeof(x) %in% parsable_atomic_types)
}
#' @export
#' @rdname is_expr
is_symbolic <- function(x) {
  x <- get_expr(x)
  typeof(x) %in% c("language", "symbol")
}

#' @rdname is_expr
#' @export
is_pairlist <- function(x) {
  typeof(x) == "pairlist"
}

#' Is object a call (language type)?
#'
#' This function tests if \code{x} is a call. This is a
#' pattern-matching predicate that will return \code{FALSE} if
#' \code{name} and \code{n} are supplied and the call does not match
#' these properties. \code{is_unary_lang()} and
#' \code{is_binary_lang()} hardcode \code{n} to 1 and 2.
#'
#' Note that the base type of calls is \code{language}, while
#' \code{call} is the old S mode. While it is usually better to avoid
#' using S terminology, it would probably be even more confusing to
#' refer to "calls" as "language". We still use \code{lang} as prefix
#' or suffix for consistency.
#'
#' @param x An object to test. If a formula, the right-hand side is
#'   extracted.
#' @param name An optional name that the call should match. It is
#'   passed to \code{\link{as_symbol}()} before matching. This argument
#'   is vectorised and you can supply a vector of names to match. In
#'   this case, \code{is_lang()} returns \code{TRUE} if at least one
#'   name matches.
#' @param n An optional number of arguments that the call should
#'   match.
#' @seealso \code{\link{is_expr}()}
#' @export
#' @examples
#' is_lang(quote(foo(bar)))
#'
#' # Right-hand sides are extracted from formulas:
#' is_lang(~foo(bar))
#'
#' # You can pattern-match the call with additional arguments:
#' is_lang(~foo(bar), "foo")
#' is_lang(~foo(bar), "bar")
#' is_lang(~foo(bar), quote(foo))
#'
#' # Match the number of arguments with is_lang():
#' is_lang(~foo(bar), "foo", 1)
#' is_lang(~foo(bar), "foo", 2)
#'
#' # Or more specifically:
#' is_unary_lang(~foo(bar))
#' is_unary_lang(~ +3)
#' is_unary_lang(~ 1 + 3)
#' is_binary_lang(~ 1 + 3)
#'
#' # Namespaced calls are a bit tricky. Strings won't work because
#' # as_symbol("base::list") returns a symbol rather than a namespace
#' # call:
#' is_lang(~base::list(baz), "base::list")
#'
#' # However you can use the fact that as_symbol(quote(base::list()))
#' # extracts the function identifier as is, and thus returns the call
#' # base::list:
#' is_lang(~base::list(baz), ~base::list(), 1)
#'
#'
#' # The name argument is vectorised so you can supply a list of names
#' # to match with:
#' is_lang(~foo(bar), c("bar", "baz"))
#' is_lang(~foo(bar), c("bar", "foo"))
#' is_lang(~base::list, c("::", ":::", "$", "@"))
is_lang <- function(x, name = NULL, n = NULL) {
  x <- get_expr(x)

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
#' @rdname is_lang
#' @export
is_language <- is_lang

#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL) {
  is_lang(x, name, n = 1L)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL) {
  is_lang(x, name, n = 2L)
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
  coerce_type(x, "symbol",
    symbol = x,
    string = symbol(x),
    quote = as_symbol(f_rhs(x)),
    language =
      if (is_prefixed_name(x)) {
        x
      } else {
        as_symbol(x[[1]])
      }
  )
}
#' @export
#' @rdname as_symbol
as_name <- function(x) {
  coerce_type(x, "name",
    string = x,
    as_string(as_symbol(x))
  )
}

#' @export
#' @rdname as_symbol
as_call <- function(x) {
  coerce_type(x, "language",
    symbol = new_lang(x),
    quote = as_call(f_rhs(x)),
    language = x,
    string = parse_expr(x)
  )
}

is_prefixed_name <- function(x) {
  fn <- x[[1]]
  if (is_symbol(fn)) {
    as_character(fn) %in% c("::", ":::", "$", "@")
  } else {
    FALSE
  }
}
