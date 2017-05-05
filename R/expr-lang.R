#' Create a call
#'
#' Language objects are (with symbols) one of the two types of
#' [symbolic][is_symbolic] objects in R. These symbolic objects form
#' the backbone of [expressions][is_expr]. They represent a value,
#' unlike literal objects which are their own values. While symbols
#' are directly [bound][env_bind] to a value, language objects
#' represent _function calls_, which is why they are commonly referred
#' to as calls.
#'
#' @section Calls as parse tree:
#'
#' Language objects are structurally identical to
#' [pairlists][pairlist]. They are containers of two objects, the head
#' and the tail (also called the CAR and the CDR).
#'
#' - The head contains the function to call, either literally or
#'   symbolically. If a literal function, the call is said to be
#'   inlined. If a symbol, the call is named. If another call, it is
#'   recursive. `foo()()` would be an example of a recursive call
#'   whose head contains another call. See [lang_type_of()] and
#'   [is_callable()].
#'
#' - The tail contains the arguments and must be a [pairlist].
#'
#' You can retrieve those components with [lang_head()] and
#' [lang_tail()]. Since language nodes can contain other nodes (either
#' calls or pairlists), they are capable of forming a tree. When R
#' [parses][parse_expr] an expression, it saves the parse tree in a
#' data structure composed of language and pairlist nodes. It is
#' precisely because the parse tree is saved in first-class R objects
#' that it is possible for functions to [capture][expr] their
#' arguments unevaluated.
#'
#' @section Call versus language:
#'
#' `call` is the old S [mode][base::mode] of these objects while
#' `language` is the R [type][base::typeof]. While it is usually
#' better to avoid using S terminology, it would probably be even more
#' confusing to systematically refer to "calls" as "language". rlang
#' still uses `lang` as particle for function dealing with calls for
#' consistency.
#'
#' @param .fn Function to call. Must be a callable object: a string,
#'   symbol, call, or a function.
#' @param ... Arguments to the call either in or out of a list. Dots
#'   are evaluated with [explicit splicing][dots_list].
#' @param .ns Namespace with which to prefix `.fn`. Must be a string
#'   or symbol.
#' @seealso lang_modify
#' @export
#' @examples
#' # fn can either be a string, a symbol or a call
#' lang("f", a = 1)
#' lang(quote(f), a = 1)
#' lang(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' lang(quote(f), a = 1, b = 2)
#' lang(quote(f), splice(list(a = 1, b = 2)))
#'
#' # Creating namespaced calls:
#' lang("fun", arg = quote(baz), .ns = "mypkg")
lang <- function(.fn, ..., .ns = NULL) {
  if (is_character(.fn)) {
    if (length(.fn) != 1) {
      abort("`.fn` must be a length 1 string")
    }
    .fn <- sym(.fn)
  } else if (!is_callable(.fn)) {
    abort("Can't create call to non-callable object")
  }

  if (!is_null(.ns)) {
    .fn <- call("::", sym(.ns), .fn)
  }

  as.call(c(.fn, dots_list(...)))
}

#' Is an object callable?
#'
#' A callable object is an object that can be set as the head of a
#' [call node][lang_head]. This includes [symbolic
#' objects][is_symbolic] that evaluate to a function or literal
#' functions.
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
#' # mut_node_car() lets you modify calls without any checking:
#' lang <- quote(foo(10))
#' mut_node_car(lang, get_env())
#'
#' # Use is_callable() to check an input object is safe to put as CAR:
#' obj <- base::identity
#'
#' if (is_callable(obj)) {
#'   lang <- mut_node_car(lang, obj)
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
#' This function tests if `x` is a call (or [language
#' object][lang]). This is a pattern-matching predicate that will
#' return `FALSE` if `name` and `n` are supplied and the call does not
#' match these properties. `is_unary_lang()` and `is_binary_lang()`
#' hardcode `n` to 1 and 2.
#'
#' @param x An object to test. If a formula, the right-hand side is
#'   extracted.
#' @param name An optional name that the call should match. It is
#'   passed to [sym()] before matching. This argument is vectorised
#'   and you can supply a vector of names to match. In this case,
#'   `is_lang()` returns `TRUE` if at least one name matches.
#' @param n An optional number of arguments that the call should
#'   match.
#' @param ns The namespace of the call. If `NULL`, the namespace
#'   doesn't participate in the pattern-matching. If an empty string
#'   `""` and `x` is a namespaced call, `is_lang()` returns
#'   `FALSE`. If any other string, `is_lang()` checks that `x` is
#'   namespaced within `ns`.
#' @seealso [is_expr()]
#' @export
#' @examples
#' is_lang(quote(foo(bar)))
#'
#' # You can pattern-match the call with additional arguments:
#' is_lang(quote(foo(bar)), "foo")
#' is_lang(quote(foo(bar)), "bar")
#' is_lang(quote(foo(bar)), quote(foo))
#'
#' # Match the number of arguments with is_lang():
#' is_lang(quote(foo(bar)), "foo", 1)
#' is_lang(quote(foo(bar)), "foo", 2)
#'
#' # Or more specifically:
#' is_unary_lang(quote(foo(bar)))
#' is_unary_lang(quote(+3))
#' is_unary_lang(quote(1 + 3))
#' is_binary_lang(quote(1 + 3))
#'
#'
#' # By default, namespaced calls are tested unqualified:
#' ns_expr <- quote(base::list())
#' is_lang(ns_expr, "list")
#'
#' # You can also specify whether the call shouldn't be namespaced by
#' # supplying an empty string:
#' is_lang(ns_expr, "list", ns = "")
#'
#' # Or if it should have a namespace:
#' is_lang(ns_expr, "list", ns = "utils")
#' is_lang(ns_expr, "list", ns = "base")
#'
#'
#' # The name argument is vectorised so you can supply a list of names
#' # to match with:
#' is_lang(quote(foo(bar)), c("bar", "baz"))
#' is_lang(quote(foo(bar)), c("bar", "foo"))
#' is_lang(quote(base::list), c("::", ":::", "$", "@"))
is_lang <- function(x, name = NULL, n = NULL, ns = NULL) {
  if (typeof(x) != "language") {
    return(FALSE)
  }

  if (!is_null(ns)) {
    if (identical(ns, "") && is_namespaced_lang(x, private = FALSE)) {
      return(FALSE)
    } else if (!is_namespaced_lang(x, ns, private = FALSE)) {
      return(FALSE)
    }
  }

  x <- lang_unnamespace(x)

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
#' @rdname is_lang
#' @export
is_unary_lang <- function(x, name = NULL, ns = NULL) {
  is_lang(x, name, n = 1L, ns = ns)
}
#' @rdname is_lang
#' @export
is_binary_lang <- function(x, name = NULL, ns = NULL) {
  is_lang(x, name, n = 2L, ns = ns)
}

#' Modify the arguments of a call
#'
#' @param .lang Can be a call (language object), a formula quoting a
#'   call in the right-hand side, or a frame object from which to
#'   extract the call expression.
#' @param ... Named or unnamed expressions (constants, names or calls)
#'   used to modify the call. Use `NULL` to remove arguments. Dots are
#'   evaluated with [explicit splicing][dots_list].
#' @param .standardise If `TRUE`, the call is standardised before hand
#'   to match existing unnamed arguments to their argument names. This
#'   prevents new named arguments from accidentally replacing original
#'   unnamed arguments.
#' @return A quosure if `.lang` is a quosure, a call otherwise.
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
#' # Supply a call frame to extract the frame expression:
#' f <- function(bool = TRUE) {
#'   lang_modify(call_frame(), splice(list(bool = FALSE)))
#' }
#' f()
#'
#'
#' # You can also modify quosures inplace:
#' f <- ~matrix(bar)
#' lang_modify(f, quote(foo))
lang_modify <- function(.lang, ..., .standardise = FALSE) {
  args <- dots_list(...)
  if (any(duplicated(names(args)) & names(args) != "")) {
    abort("Duplicate arguments")
  }

  if (.standardise) {
    quo <- lang_as_quosure(.lang, caller_env())
    expr <- get_expr(lang_standardise(quo))
  } else {
    expr <- get_expr(.lang)
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

  set_expr(.lang, expr)
}
lang_as_quosure <- function(lang, env) {
  if (is_frame(lang)) {
    new_quosure(lang$expr, lang$env)
  } else {
    as_quosure(lang, env)
  }
}

#' Standardise a call
#'
#' This is essentially equivalent to [base::match.call()], but with
#' better handling of primitive functions.
#'
#' @param lang Can be a call (language object), a formula quoting a
#'   call in the right-hand side, or a frame object from which to
#'   extract the call expression.
#' @return A quosure if `.lang` is a quosure, a raw call otherwise.
#' @export
lang_standardise <- function(lang) {
  expr <- get_expr(lang)
  if (is_frame(lang)) {
    fn <- lang$fn
  } else {
    # The call name might be a literal, not necessarily a symbol
    env <- get_env(lang, caller_env())
    fn <- eval_bare(lang_head(expr), env)
  }

  matched <- match.call(as_closure(fn), expr)
  set_expr(lang, matched)
}

#' Extract function from a call
#'
#' If a frame or formula, the function will be retrieved from the
#' associated environment. Otherwise, it is looked up in the calling
#' frame.
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
#' test <- function() lang_fn(call_frame())
#' test()
lang_fn <- function(lang) {
  if (is_frame(lang)) {
    return(lang$fn)
  }

  expr <- get_expr(lang)
  env <- get_env(lang, caller_env())

  if (!is_lang(expr)) {
    abort("`lang` must quote a lang")
  }

  switch_lang(expr,
    recursive = abort("`lang` does not lang a named or inlined function"),
    inlined = node_car(expr),
    named = ,
    namespaced = ,
    eval_bare(node_car(expr), env)
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
#' # Or from a frame:
#' foo <- function(bar) lang_name(call_frame())
#' foo(bar)
#'
#' # Namespaced calls are correctly handled:
#' lang_name(~base::matrix(baz))
#'
#' # Anonymous and subsetted functions return NULL:
#' lang_name(~foo$bar())
#' lang_name(~foo[[bar]]())
#' lang_name(~foo()())
lang_name <- function(lang) {
  lang <- get_expr(lang)
  if (!is_lang(lang)) {
    abort("`lang` must be a call or must wrap a call (e.g. in a quosure)")
  }

  switch_lang(lang,
    named = as_string(node_car(lang)),
    namespaced = as_string(node_cadr(node_cdar(lang))),
    NULL
  )
}

#' Return the head or tail of a call object
#'
#' @description
#'
#' These functions return the head or the tail of a call. See section
#' on calls as parse trees in [lang()]. They are equivalent to
#' [node_car()] and [node_cdr()] but support quosures and check that
#' the input is indeed a call before retrieving the head or tail (it
#' is unsafe to do this without type checking).
#'
#' `lang_head()` returns the head of the call without any conversion,
#' unlike [lang_name()] which checks that the head is a symbol and
#' converts it to a string. `lang_tail()` returns the pairlist of
#' arguments (while [lang_args()] returns the same object converted to
#' a regular list)
#'
#' @inheritParams lang_standardise
#' @seealso [pairlist], [lang_args()], [lang()]
#' @export
#' @examples
#' lang <- quote(foo(bar, baz))
#' lang_head(lang)
#' lang_tail(lang)
lang_head <- function(lang) {
  lang <- get_expr(lang)
  stopifnot(is_lang(lang))
  node_car(lang)
}
#' @rdname lang_head
#' @export
lang_tail <- function(lang) {
  lang <- get_expr(lang)
  stopifnot(is_lang(lang))
  node_cdr(lang)
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
#' # Subsetting a call returns the arguments converted to a language
#' # object:
#' call[-1]
#'
#' # See also lang_tail() which returns the arguments without
#' # conversion as the original pairlist:
#' str(lang_tail(call))
#'
#' # On the other hand, lang_args() returns a regular list that is
#' # often easier to work with:
#' str(lang_args(call))
#'
#' # When the arguments are unnamed, a vector of empty strings is
#' # supplied (rather than NULL):
#' lang_args_names(call)
lang_args <- function(lang) {
  lang <- get_expr(lang)
  args <- as.list(lang_tail(lang))
  set_names((args), names2(args))
}

#' @rdname lang_args
#' @export
lang_args_names <- function(lang) {
  lang <- get_expr(lang)
  names2(lang_tail(lang))
}

is_qualified_lang <- function(x) {
  if (typeof(x) != "language") return(FALSE)
  is_qualified_symbol(node_car(x))
}
is_namespaced_lang <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") return(FALSE)
  if (!is_namespaced_symbol(node_car(x), ns, private)) return(FALSE)
  TRUE
}

# Returns a new call whose CAR has been unqualified
lang_unnamespace <- function(x) {
  if (is_namespaced_lang(x)) {
    lang <- lang(node_cadr(node_cdar(x)))
    mut_node_cdr(lang, node_cdr(x))
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
  identical(qualifier, sym_namespace) ||
    identical(qualifier, sym_namespace2) ||
    identical(qualifier, sym_dollar) ||
    identical(qualifier, sym_at)
}
is_namespaced_symbol <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") return(FALSE)
  if (!is_null(ns) && !identical(node_cadr(x), sym(ns))) return(FALSE)

  head <- node_car(x)
  if (is_null(private)) {
    identical(head, sym_namespace) || identical(head, sym_namespace2)
  } else if (private) {
    identical(head, sym_namespace2)
  } else {
    identical(head, sym_namespace)
  }
}
