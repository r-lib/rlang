#' Create a call
#'
#' @description
#'
#' Language objects are (with symbols) one of the two types of
#' [symbolic][is_symbolic] objects in R. These symbolic objects form
#' the backbone of [expressions][is_expression]. They represent a value,
#' unlike literal objects which are their own values. While symbols
#' are directly [bound][env_bind] to a value, language objects
#' represent _function calls_, which is why they are commonly referred
#' to as calls.
#'
#' `call2()` creates a call from a function name (or a literal
#' function to inline in the call) and a list of arguments.
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0 `lang()` was soft-deprecated and renamed to
#' `call2()`.
#'
#' In early versions of rlang calls were called "language" objects in
#' order to follow the R type nomenclature as returned by
#' [base::typeof()]. The goal was to avoid adding to the confusion
#' between S modes and R types. With hindsight we find it is better to
#' use more meaningful type names.
#'
#' @param .fn Function to call. Must be a callable object: a string,
#'   symbol, call, or a function.
#' @param ... Arguments to the call either in or out of a list. These dots
#'   support [tidy dots][tidy-dots] features.
#' @param .ns Namespace with which to prefix `.fn`. Must be a string
#'   or symbol.
#' @seealso call_modify
#' @export
#' @examples
#' # fn can either be a string, a symbol or a call
#' call2("f", a = 1)
#' call2(quote(f), a = 1)
#' call2(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' call2(quote(f), a = 1, b = 2)
#' call2(quote(f), splice(list(a = 1, b = 2)))
#'
#' # Creating namespaced calls:
#' call2("fun", arg = quote(baz), .ns = "mypkg")
call2 <- function(.fn, ..., .ns = NULL) {
  if (is_character(.fn)) {
    if (length(.fn) != 1) {
      abort("`.fn` must be a length 1 string")
    }
    .fn <- sym(.fn)
  } else if (!is_callable(.fn)) {
    abort("Can't create call to non-callable object")
  }

  if (!is_null(.ns)) {
    .fn <- new_call(namespace_sym, pairlist(sym(.ns), .fn))
  }

  new_call(.fn, as.pairlist(dots_list(...)))
}

#' Is an object callable?
#'
#' A callable object is an object that can appear in the function
#' position of a call (as opposed to argument position). This includes
#' [symbolic objects][is_symbolic] that evaluate to a function or
#' literal functions embedded in the call.
#'
#' Note that strings may look like callable objects because
#' expressions of the form `"list"()` are valid R code. However,
#' that's only because the R parser transforms strings to symbols. It
#' is not legal to manually set language heads to strings.
#'
#' @param x An object to test.
#' @export
#' @examples
#' # Symbolic objects and functions are callable:
#' is_callable(quote(foo))
#' is_callable(base::identity)
#'
#' # node_poke_car() lets you modify calls without any checking:
#' lang <- quote(foo(10))
#' node_poke_car(lang, get_env())
#'
#' # Use is_callable() to check an input object is safe to put as CAR:
#' obj <- base::identity
#'
#' if (is_callable(obj)) {
#'   lang <- node_poke_car(lang, obj)
#' } else {
#'   abort("`obj` must be callable")
#' }
#'
#' eval_bare(lang)
is_callable <- function(x) {
  is_symbolic(x) || is_function(x)
}

#' Is object a call?
#'
#' This function tests if `x` is a [call][call2]. This is a
#' pattern-matching predicate that returns `FALSE` if `name` and `n`
#' are supplied and the call does not match these properties.
#' `is_unary_call()` and `is_binary_call()` hardcode `n` to 1 and 2.
#'
#'
#' @section Life cycle:
#'
#' `is_lang()` has been soft-deprecated and renamed to `is_call()` in
#' rlang 0.2.0 and similarly for `is_unary_lang()` and
#' `is_binary_lang()`. This renaming follows the general switch from
#' "language" to "call" in the rlang type nomenclature. See lifecycle
#' section in [call2()].
#'
#' @param x An object to test. If a formula, the right-hand side is
#'   extracted.
#' @param name An optional name that the call should match. It is
#'   passed to [sym()] before matching. This argument is vectorised
#'   and you can supply a vector of names to match. In this case,
#'   `is_call()` returns `TRUE` if at least one name matches.
#' @param n An optional number of arguments that the call should
#'   match.
#' @param ns The namespace of the call. If `NULL`, the namespace
#'   doesn't participate in the pattern-matching. If an empty string
#'   `""` and `x` is a namespaced call, `is_call()` returns
#'   `FALSE`. If any other string, `is_call()` checks that `x` is
#'   namespaced within `ns`.
#' @seealso [is_expression()]
#' @export
#' @examples
#' is_call(quote(foo(bar)))
#'
#' # You can pattern-match the call with additional arguments:
#' is_call(quote(foo(bar)), "foo")
#' is_call(quote(foo(bar)), "bar")
#' is_call(quote(foo(bar)), quote(foo))
#'
#' # Match the number of arguments with is_call():
#' is_call(quote(foo(bar)), "foo", 1)
#' is_call(quote(foo(bar)), "foo", 2)
#'
#'
#' # By default, namespaced calls are tested unqualified:
#' ns_expr <- quote(base::list())
#' is_call(ns_expr, "list")
#'
#' # You can also specify whether the call shouldn't be namespaced by
#' # supplying an empty string:
#' is_call(ns_expr, "list", ns = "")
#'
#' # Or if it should have a namespace:
#' is_call(ns_expr, "list", ns = "utils")
#' is_call(ns_expr, "list", ns = "base")
#'
#'
#' # The name argument is vectorised so you can supply a list of names
#' # to match with:
#' is_call(quote(foo(bar)), c("bar", "baz"))
#' is_call(quote(foo(bar)), c("bar", "foo"))
#' is_call(quote(base::list), c("::", ":::", "$", "@"))
is_call <- function(x, name = NULL, n = NULL, ns = NULL) {
  if (typeof(x) != "language") {
    return(FALSE)
  }

  if (!is_null(ns)) {
    if (identical(ns, "") && is_namespaced_call(x, private = FALSE)) {
      return(FALSE)
    } else if (!is_namespaced_call(x, ns, private = FALSE)) {
      return(FALSE)
    }
  }

  x <- call_unnamespace(x)

  if (!is_null(name)) {
    # Wrap language objects in a list
    if (!is_vector(name)) {
      name <- list(name)
    }

    unmatched <- TRUE
    for (elt in name) {
      if (identical(x[[1]], sym(elt))) {
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


#' Modify the arguments of a call
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_modify()` was soft-deprecated and renamed to
#' `call_modify()`. See lifecycle section in [call2()] for more about
#' this change.
#'
#' @param .call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression.
#' @param ... Named or unnamed expressions (constants, names or calls)
#'   used to modify the call. Use `NULL` to remove arguments. These
#'   dots support [tidy dots][tidy-dots] features.
#' @param .standardise If `TRUE`, the call is standardised beforehand
#'   to match existing unnamed arguments to their argument names. This
#'   prevents new named arguments from accidentally replacing original
#'   unnamed arguments.
#' @param .env The environment where to find the `call` definition in
#'   case `call` is not wrapped in a quosure. This is passed to
#'   `call_standardise()` if `.standardise` is `TRUE`.
#'
#' @return A quosure if `.call` is a quosure, a call otherwise.
#' @seealso lang
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#'
#' # Modify an existing argument
#' call_modify(call, na.rm = FALSE)
#' call_modify(call, x = quote(y))
#'
#' # Remove an argument
#' call_modify(call, na.rm = NULL)
#'
#' # Add a new argument
#' call_modify(call, trim = 0.1)
#'
#' # Add an explicit missing argument
#' call_modify(call, na.rm = quote(expr = ))
#'
#' # Supply a list of new arguments with `!!!`
#' newargs <- list(na.rm = NULL, trim = 0.1)
#' call_modify(call, !!! newargs)
#'
#' # Supply a call frame to extract the frame expression:
#' f <- function(bool = TRUE) {
#'   call_modify(call_frame(), splice(list(bool = FALSE)))
#' }
#' f()
#'
#'
#' # You can also modify quosures inplace:
#' f <- quo(matrix(bar))
#' call_modify(f, quote(foo))
call_modify <- function(.call, ...,
                        .standardise = FALSE,
                        .env = caller_env()) {
  args <- dots_list(...)
  if (any(duplicated(names(args)) & names(args) != "")) {
    abort("Duplicate arguments")
  }

  if (.standardise) {
    expr <- get_expr(call_standardise(.call, env = .env))
  } else {
    expr <- get_expr(.call)
  }

  # Named arguments can be spliced by R
  named <- have_name(args)
  for (nm in names(args)[named]) {
    expr[[nm]] <- args[[nm]]
  }

  if (any(!named)) {
    # Duplicate list structure in case it wasn't before
    if (!any(named)) {
      expr <- duplicate(expr, shallow = TRUE)
    }

    remaining_args <- as.pairlist(args[!named])
    expr <- node_append(expr, remaining_args)
  }

  set_expr(.call, expr)
}

#' Standardise a call
#'
#' This is essentially equivalent to [base::match.call()], but with
#' experimental handling of primitive functions.
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_standardise()` was soft-deprecated and
#' renamed to `call_standardise()`. See lifecycle section in [call2()]
#' for more about this change.
#'
#' @param call Can be a call or a quosure that wraps a call.
#' @param env The environment where to find the definition of the
#'   function quoted in `call` in case `call` is not wrapped in a
#'   quosure.
#'
#' @return A quosure if `call` is a quosure, a raw call otherwise.
#' @export
call_standardise <- function(call, env = caller_env()) {
  expr <- get_expr(call)
  if (is_frame(call)) {
    fn <- call$fn
  } else {
    # The call name might be a literal, not necessarily a symbol
    env <- get_env(call, env)
    fn <- eval_bare(node_car(expr), env)
  }

  matched <- match.call(as_closure(fn), expr)
  set_expr(call, matched)
}

#' Extract function from a call
#'
#' If a frame or formula, the function will be retrieved from the
#' associated environment. Otherwise, it is looked up in the calling
#' frame.
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_fn()` was soft-deprecated and renamed to
#' `call_fn()`. See lifecycle section in [call2()] for more about this
#' change.
#'
#' @inheritParams call_standardise
#' @export
#' @seealso [call_name()]
#' @examples
#' # Extract from a quoted call:
#' call_fn(quote(matrix()))
#' call_fn(quo(matrix()))
#'
#' # Extract the calling function
#' test <- function() call_fn(call_frame())
#' test()
call_fn <- function(call, env = caller_env()) {
  if (is_frame(call)) {
    return(call$fn)
  }

  expr <- get_expr(call)
  env <- get_env(call, env)

  if (!is_call(expr)) {
    abort("`call` must quote a call")
  }

  switch_lang(expr,
    recursive = abort("`call` does not call a named or inlined function"),
    inlined = node_car(expr),
    named = ,
    namespaced = ,
    eval_bare(node_car(expr), env)
  )
}

#' Extract function name of a call
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_name()` was soft-deprecated and renamed to
#' `call_name()`. See lifecycle section in [call2()] for more about this
#' change.
#'
#' @inheritParams call_standardise
#' @return A string with the function name, or `NULL` if the function
#'   is anonymous.
#' @seealso [call_fn()]
#' @export
#' @examples
#' # Extract the function name from quoted calls:
#' call_name(quote(foo(bar)))
#' call_name(quo(foo(bar)))
#'
#' # Or from a frame:
#' foo <- function(bar) call_name(call_frame())
#' foo(bar)
#'
#' # Namespaced calls are correctly handled:
#' call_name(~base::matrix(baz))
#'
#' # Anonymous and subsetted functions return NULL:
#' call_name(quote(foo$bar()))
#' call_name(quote(foo[[bar]]()))
#' call_name(quote(foo()()))
call_name <- function(call) {
  call <- get_expr(call)
  if (!is_call(call)) {
    abort("`call` must be a call or must wrap a call (e.g. in a quosure)")
  }

  switch_lang(call,
    named = as_string(node_car(call)),
    namespaced = as_string(node_cadr(node_cdar(call))),
    NULL
  )
}

#' Extract arguments from a call
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_args()` and `lang_args_names()` were
#' soft-deprecated and renamed to `call_args()` and
#' `call_args_names()`. See lifecycle section in [call2()] for more
#' about this change.
#'
#' @inheritParams call_standardise
#' @return A named list of arguments.
#' @seealso [fn_fmls()] and [fn_fmls_names()]
#' @export
#' @examples
#' call <- quote(f(a, b))
#'
#' # Subsetting a call returns the arguments converted to a language
#' # object:
#' call[-1]
#'
#' # On the other hand, call_args() returns a regular list that is
#' # often easier to work with:
#' str(call_args(call))
#'
#' # When the arguments are unnamed, a vector of empty strings is
#' # supplied (rather than NULL):
#' call_args_names(call)
call_args <- function(call) {
  call <- get_expr(call)
  args <- as.list(call[-1])
  set_names((args), names2(args))
}
#' @rdname call_args
#' @export
call_args_names <- function(call) {
  call <- get_expr(call)
  names2(call[-1])
}

is_qualified_call <- function(x) {
  if (typeof(x) != "language") return(FALSE)
  is_qualified_symbol(node_car(x))
}
is_namespaced_call <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") return(FALSE)
  if (!is_namespaced_symbol(node_car(x), ns, private)) return(FALSE)
  TRUE
}

# Returns a new call whose CAR has been unqualified
call_unnamespace <- function(x) {
  if (is_namespaced_call(x)) {
    call <- call2(node_cadr(node_cdar(x)))
    node_poke_cdr(call, node_cdr(x))
  } else {
    x
  }
}

# Qualified and namespaced symbols are actually calls
is_qualified_symbol <- function(x) {
  if (typeof(x) != "language") return(FALSE)

  head <- node_cadr(node_cdr(x))
  if (typeof(head) != "symbol") return(FALSE)

  qualifier <- node_car(x)
  identical(qualifier, namespace_sym) ||
    identical(qualifier, namespace2_sym) ||
    identical(qualifier, dollar_sym) ||
    identical(qualifier, at_sym)
}
is_namespaced_symbol <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") return(FALSE)
  if (!is_null(ns) && !identical(node_cadr(x), sym(ns))) return(FALSE)

  head <- node_car(x)
  if (is_null(private)) {
    identical(head, namespace_sym) || identical(head, namespace2_sym)
  } else if (private) {
    identical(head, namespace2_sym)
  } else {
    identical(head, namespace_sym)
  }
}

which_operator <- function(call) {
  .Call(rlang_which_operator, call)
}
call_has_precedence <- function(call, parent_call, side = NULL) {
  .Call(rlang_call_has_precedence, call, parent_call, side)
}
