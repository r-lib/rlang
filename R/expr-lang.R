#' Is object a call (language type)?
#'
#' This function tests if `x` is a call. This is a pattern-matching
#' predicate that will return `FALSE` if `name` and `n` are supplied
#' and the call does not match these properties. `is_unary_lang()` and
#' `is_binary_lang()` hardcode `n` to 1 and 2.
#'
#' Note that the base type of calls is `language`, while `call` is the
#' old S mode. While it is usually better to avoid using S
#' terminology, it would probably be even more confusing to refer to
#' "calls" as "language". We still use `lang` as prefix or suffix for
#' consistency.
#'
#' @param x An object to test. If a formula, the right-hand side is
#'   extracted.
#' @param name An optional name that the call should match. It is
#'   passed to [as_symbol()] before matching. This argument is
#'   vectorised and you can supply a vector of names to match. In this
#'   case, `is_lang()` returns `TRUE` if at least one name matches.
#' @param n An optional number of arguments that the call should
#'   match.
#' @seealso [is_expr()]
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
is_unary_lang <- function(x, name = NULL) {
  is_lang(x, name, n = 1L)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL) {
  is_lang(x, name, n = 2L)
}


#' @export
#' @rdname as_symbol
as_lang <- function(x) {
  coerce_type(x, "language",
    symbol = new_language(x),
    quosure = as_lang(f_rhs(x)),
    string = parse_expr(x),
    language = x
  )
}

#' Create a language call by "hand"
#'
#' @param .fn Function to call. Can be a string or symbol or any
#'   object supported by [as_symbol()].
#' @param ... Arguments to the call either in or out of a list. Dots
#'   are evaluated with [explicit splicing][dots_list].
#' @param .ns Namespace with which to prefix `.fn`. Can be a string or
#'   symbol or any object supported by [as_symbol()].
#' @seealso lang_modify
#' @export
#' @examples
#' # fn can either be a string, a symbol or a call
#' new_language("f", a = 1)
#' new_language(quote(f), a = 1)
#' new_language(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' new_language(quote(f), a = 1, b = 2)
#' new_language(quote(f), splice(list(a = 1, b = 2)))
#'
#' # Creating namespaced calls:
#' new_language("fun", arg = quote(baz), .ns = "mypkg")
new_language <- function(.fn, ..., .ns = NULL) {
  if (is_character(.fn)) {
    if (length(.fn) != 1) {
      abort("Character `.fn` must be length 1")
    }
    .fn <- as_symbol(.fn)
  }

  if (!is_null(.ns)) {
    .ns <- as_symbol(.ns)
    .fn <- call("::", .ns, .fn)
  }

  as.call(c(.fn, dots_list(...)))
}

#' Modify the arguments of a call.
#'
#' @param .call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @param ... Named or unnamed expressions (constants, names or calls)
#'   used to modify the call. Use `NULL` to remove arguments. Dots are
#'   evaluated with [explicit splicing][dots_list].
#' @param .standardise If `TRUE`, the call is standardised before hand
#'   to match existing unnamed arguments to their argument names. This
#'   prevents new named arguments from accidentally replacing original
#'   unnamed arguments.
#' @return A tidy quote if `.call` is a tidy quote, a call otherwise.
#' @seealso lang
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#'
#' # Modify an existing argument
#' lang_modify(call, na.rm = FALSE)
#' lang_modify(call, x = quote(y))
#'
#' # Remove an argument
#' lang_modify(call, na.rm = NULL)
#'
#' # Add a new argument
#' lang_modify(call, trim = 0.1)
#'
#' # Add an explicit missing argument
#' lang_modify(call, na.rm = quote(expr = ))
#'
#' # Supply a list of new arguments with splice()
#' newargs <- list(na.rm = NULL, trim = 0.1)
#' lang_modify(call, splice(newargs))
#'
#' # If the call is missing, the parent frame is used instead.
#' f <- function(bool = TRUE) lang_modify(, splice(list(bool = FALSE)))
#' f()
#'
#'
#' # You can also modify quosures inplace:
#' f <- ~matrix(bar)
#' lang_modify(f, quote(foo))
lang_modify <- function(.call = caller_frame(), ..., .standardise = FALSE) {
  args <- dots_list(...)
  if (any(duplicated(names(args)) & names(args) != "")) {
    abort("Duplicate arguments")
  }

  orig <- as_generic_expr(.call)

  if (.standardise) {
    quote <- as_quosure(.call, caller_env())
    quote <- set_expr(quote, as_lang(quote))
    quote <- lang_standardise(quote)
    call <- get_expr(quote)
  } else {
    call <- as_lang(get_expr(.call))
  }

  # Named arguments can be spliced by R
  named <- have_name(args)
  for (nm in names(args)[named]) {
    call[[nm]] <- args[[nm]]
  }

  if (any(!named)) {
    # Duplicate list structure in case it wasn't before
    if (!any(named)) {
      call <- duplicate(call, shallow = TRUE)
    }

    remaining_args <- as.pairlist(args[!named])
    call <- node_append(call, remaining_args)
  }

  set_expr(orig, call)
}

#' Standardise a call.
#'
#' This is essentially equivalent to [base::match.call()], but handles
#' primitive functions more gracefully.
#'
#' @param call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used.
#' @seealso [lang_homogenise()] for a version more suitable to
#'   language analysis.
#' @return A tidy quote if `.call` is a tidy quote, a call otherwise.
#' @export
lang_standardise <- function(call = caller_frame()) {
  orig <- as_generic_expr(call)
  quote <- as_quosure(call, caller_env())

  # The call name might be a literal, not necessarily a symbol
  fn <- lang_name(quote)
  fn <- switch_type(fn,
    string = get(fn, envir = f_env(quote), mode = "function"),
    primitive = ,
    closure = fn,
    abort("Cannot extract a function to compare the call to")
  )

  matched <- match.call(as_closure(fn), get_expr(quote))
  set_expr(orig, matched)
}

#' Extract function from a call
#'
#' If a frame or formula, the function will be retrieved from their
#' environment. Otherwise, it is looked up in the calling frame.
#'
#' @inheritParams lang_standardise
#' @export
#' @seealso [lang_name()]
#' @examples
#' # Extract from a quoted call:
#' lang_fn(~matrix())
#' lang_fn(quote(matrix()))
#'
#' # Extract the calling function
#' test <- function() lang_fn()
#' test()
lang_fn <- function(call = caller_frame()) {
  if (is_frame(call)) {
    return(call$fn)
  }

  call <- as_quosure(call, caller_env())
  expr <- f_rhs(call)

  if (!is_lang(expr)) {
    abort("`call` must quote a call")
  }

  switch_lang(expr,
    recursive = abort("`call` does not call a named or inlined function"),
    inlined = node_car(expr),
    named = ,
    namespaced = ,
    eval_bare(node_car(expr), f_env(call))
  )
}

#' Extract function name of a call
#'
#' @inheritParams lang_standardise
#' @return A string with the function name, or `NULL` if the function
#'   is anonymous.
#' @seealso [lang_fn()]
#' @export
#' @examples
#' # Extract the function name from quoted calls:
#' lang_name(~foo(bar))
#' lang_name(quote(foo(bar)))
#'
#' # The calling expression is used as default:
#' foo <- function(bar) lang_name()
#' foo(bar)
#'
#' # Namespaced calls are correctly handled:
#' lang_name(~base::matrix(baz))
#'
#' # Anonymous and subsetted functions return NULL:
#' lang_name(~foo$bar())
#' lang_name(~foo[[bar]]())
#' lang_name(~foo()())
lang_name <- function(call = caller_frame()) {
  call <- get_expr(call)
  if (!is_lang(call)) {
    abort("`call` must be a call or a tidy quote of a call")
  }

  switch_lang(call,
    named = as_string(node_car(call)),
    namespaced = as_string(node_cadr(node_cdar(call))),
    NULL
  )
}

#' Return the head or tail of a call object.
#'
#' @description
#'
#' Internally, calls are structured as a tree of expressions (see
#' [switch_lang()] and [pairlist] documentation pages). A `new_language` object
#' is the top level node of the tree. `lang_head()` and `lang_tail()`
#' allow you to retrieve the node components.
#'
#' * `lang_head()` Its head (the CAR of the node) usually contains a
#'   symbol in case of a call to a named function. However it could be
#'   other things, like another call (e.g. `foo()()`). Thus it is like
#'   [lang_name()], but returns the head without any type checking or
#'   conversion (whereas `lang_name()` checks that the head is a
#'   symbol and converts it to a string).
#'
#' * The second component of the tree node contains the arguments. The
#'   type of arguments is _pairlist_. Pairlists are actually
#'   equivalent to `language` objects (calls), they just have a
#'   different name. `lang_tail()` returns the pairlist of arguments
#'   ([lang_args()] returns the same object converted to a regular
#'   list).
#'
#' @inheritParams lang_standardise
#' @seealso [pairlist], [lang_args()]
#' @export
#' @examples
#' lang <- quote(foo(bar, baz))
#' lang_head(lang)
#' lang_tail(lang)
lang_head <- function(call = caller_frame()) {
  call <- get_expr(call)
  stopifnot(is_lang(call))
  node_car(call)
}
#' @rdname lang_head
#' @export
lang_tail <- function(call = caller_frame()) {
  call <- get_expr(call)
  stopifnot(is_lang(call))
  node_cdr(call)
}

#' Extract arguments from a call
#'
#' @inheritParams lang_standardise
#' @return A named list of arguments.
#' @seealso [lang_tail()], [fn_fmls()] and [fn_fmls_names()]
#' @export
#' @examples
#' call <- quote(f(a, b))
#'
#' # Subsetting a call returns the arguments in a language pairlist:
#' call[-1]
#'
#' # Whereas lang_args() returns a list:
#' lang_args(call)
#'
#' # When the call arguments are supplied without names, a vector of
#' # empty strings is supplied (rather than NULL):
#' lang_args_names(call)
lang_args <- function(call = caller_frame()) {
  call <- get_expr(call)
  args <- as.list(lang_tail(call))
  set_names((args), names2(args))
}

#' @rdname lang_args
#' @export
lang_args_names <- function(call = caller_frame()) {
  call <- get_expr(call)
  names2(lang_tail(call))
}

is_qualified_lang <- function(x) {
  if (typeof(x) != "language") return(FALSE)
  is_qualified_symbol(node_car(x))
}
is_namespaced_lang <- function(x) {
  if (typeof(x) != "language") return(FALSE)
  is_namespaced_symbol(node_car(x))
}

# Qualified and namespaced symbols are actually calls
is_qualified_symbol <- function(x) {
  if (typeof(x) != "language") return(FALSE)

  head <- node_cadr(node_cdr(x))
  if (typeof(head) != "symbol") return(FALSE)

  qualifier <- node_car(x)
  identical(qualifier, sym_namespace) ||
    identical(qualifier, sym_namespace2) ||
    identical(qualifier, sym_dollar) ||
    identical(qualifier, sym_at)
}
is_namespaced_symbol <- function(x) {
  if (typeof(x) != "language") return(FALSE)

  qualifier <- node_car(x)
  identical(qualifier, sym_namespace) ||
    identical(qualifier, sym_namespace2)
}
