#' Create a call
#'
#' @description
#'
#' Language objects are (with symbols) one of the two types of
#' [symbolic][is_symbolic] objects in R. These symbolic objects form
#' the backbone of [expressions][is_expr]. They represent a value,
#' unlike literal objects which are their own values. While symbols
#' are directly [bound][env_bind] to a value, language objects
#' represent _function calls_, which is why they are commonly referred
#' to as calls.
#'
#' * `call2()` creates a call from a function name (or a literal
#'   function to inline in the call) and a list of arguments.
#'
#' * `new_call()` is bare-bones and takes a head and a tail. The
#'   head must be [callable][is_callable] and the tail must be a
#'   [pairlist]. See section on calls as parse trees below. This
#'   constructor is useful to avoid costly coercions between lists and
#'   pairlists of arguments.
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
#' You can retrieve those components with [call_head()] and
#' [call_tail()]. Since language nodes can contain other nodes (either
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
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0:
#'
#' * `lang()` was soft-deprecated and renamed to `call2()`
#' * `new_language()` was soft-deprecated and renamed to `new_call()`
#'
#' In rlang 0.1.0 calls were called "language" objects in order to
#' follow the R type nomenclature as returned by [base::typeof()]. We
#' wanted to avoid adding to the confusion between S modes and R
#' types. With hindsight we find it is better to use more meaningful
#' type names.
#'
#' @param .fn Function to call. Must be a callable object: a string,
#'   symbol, call, or a function.
#' @param ... Arguments to the call either in or out of a list. Dots
#'   are evaluated with [explicit splicing][dots_list].
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
    .fn <- new_call(sym_namespace, pairlist(sym(.ns), .fn))
  }

  new_call(.fn, as.pairlist(dots_list(...)))
}
#' @rdname call2
#' @param head A [callable][is_callable] object: a symbol, call, or
#'   literal function.
#' @param tail A [node list][pairlist] of arguments.
#' @export
new_call <- function(head, tail = NULL) {
  if (!is_callable(head)) {
    abort("Can't create call to non-callable object")
  }
  if (!is_node_list(tail)) {
    abort("`tail` must be a node list")
  }
  .Call(rlang_new_call, head, tail)
}

#' Is an object callable?
#'
#' A callable object is an object that can be set as the head of a
#' [call node][call_head]. This includes [symbolic
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
#' @seealso [is_expr()]
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
#' # Or more specifically:
#' is_unary_call(quote(foo(bar)))
#' is_unary_call(quote(+3))
#' is_unary_call(quote(1 + 3))
#' is_binary_call(quote(1 + 3))
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
    if (identical(ns, "") && is_namespaced_lang(x, private = FALSE)) {
      return(FALSE)
    } else if (!is_namespaced_lang(x, ns, private = FALSE)) {
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
#' @rdname is_lang
#' @export
is_unary_call <- function(x, name = NULL, ns = NULL) {
  is_call(x, name, n = 1L, ns = ns)
}
#' @rdname is_lang
#' @export
is_binary_call <- function(x, name = NULL, ns = NULL) {
  is_call(x, name, n = 2L, ns = ns)
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
#'   used to modify the call. Use `NULL` to remove arguments. Dots are
#'   evaluated with [explicit splicing][dots_list].
#' @param .standardise If `TRUE`, the call is standardised beforehand
#'   to match existing unnamed arguments to their argument names. This
#'   prevents new named arguments from accidentally replacing original
#'   unnamed arguments.
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
call_modify <- function(.call, ..., .standardise = FALSE) {
  args <- dots_list(...)
  if (any(duplicated(names(args)) & names(args) != "")) {
    abort("Duplicate arguments")
  }

  if (.standardise) {
    quo <- call_as_quosure(.call, caller_env())
    expr <- get_expr(call_standardise(quo))
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
call_as_quosure <- function(call, env) {
  if (is_frame(call)) {
    new_quosure(call$expr, call$env)
  } else {
    as_quosure(call, env)
  }
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
#' @return A quosure if `call` is a quosure, a raw call otherwise.
#' @export
call_standardise <- function(call) {
  expr <- get_expr(call)
  if (is_frame(call)) {
    fn <- call$fn
  } else {
    # The call name might be a literal, not necessarily a symbol
    env <- get_env(call, caller_env())
    fn <- eval_bare(call_head(expr), env)
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
call_fn <- function(call) {
  if (is_frame(call)) {
    return(call$fn)
  }

  expr <- get_expr(call)
  env <- get_env(call, caller_env())

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

#' Return the head or tail of a call
#'
#' @description
#'
#' These functions return the head or the tail of a call. See section
#' on calls as parse trees in [call2()]. They are equivalent to
#' [node_car()] and [node_cdr()] but support quosures and check that
#' the input is indeed a call before retrieving the head or tail (it
#' is unsafe to do this without type checking).
#'
#' `call_head()` returns the head of the call without any conversion,
#' unlike [call_name()] which checks that the head is a symbol and
#' converts it to a string. `call_tail()` returns the pairlist of
#' arguments (while [lang_args()] returns the same object converted to
#' a regular list)
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_head()` and `lang_tail()` were
#' soft-deprecated and renamed to `call_head()` and `call_tail()`. See
#' lifecycle section in [call2()] for more about this change.
#'
#' @inheritParams call_standardise
#' @seealso [pairlist], [lang_args()], [call2()]
#' @export
#' @examples
#' call <- quote(foo(bar, baz))
#' call_head(call)
#' call_tail(call)
call_head <- function(call) {
  call <- get_expr(call)
  stopifnot(is_call(call))
  node_car(call)
}
#' @rdname call_head
#' @export
call_tail <- function(call) {
  call <- get_expr(call)
  stopifnot(is_call(call))
  node_cdr(call)
}

#' Extract arguments from a call
#'
#' @inheritParams lang_standardise
#' @return A named list of arguments.
#' @seealso [call_tail()], [fn_fmls()] and [fn_fmls_names()]
#' @export
#' @examples
#' call <- quote(f(a, b))
#'
#' # Subsetting a call returns the arguments converted to a language
#' # object:
#' call[-1]
#'
#' # See also call_tail() which returns the arguments without
#' # conversion as the original pairlist:
#' str(call_tail(call))
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
  args <- as.list(call_tail(lang))
  set_names((args), names2(args))
}

#' @rdname lang_args
#' @export
lang_args_names <- function(lang) {
  lang <- get_expr(lang)
  names2(call_tail(lang))
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
call_unnamespace <- function(x) {
  if (is_namespaced_lang(x)) {
    call <- call2(node_cadr(node_cdar(x)))
    mut_node_cdr(call, node_cdr(x))
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

which_operator <- function(call) {
  .Call(rlang_which_operator, call)
}
call_has_precedence <- function(call, parent_call, side = NULL) {
  .Call(rlang_call_has_precedence, call, parent_call, side)
}
