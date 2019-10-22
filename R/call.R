#' Create a call
#'
#' @description
#'
#' Quoted function calls are one of the two types of
#' [symbolic][is_symbolic] objects in R. They represent the action of
#' calling a function, possibly with arguments. There are two ways of
#' creating a quoted call:
#'
#' * By [quoting][quotation] it. Quoting prevents functions from being
#'   called. Instead, you get the description of the function call as
#'   an R object. That is, a quoted function call.
#'
#' * By constructing it with [base::call()], [base::as.call()], or
#'   `call2()`. In this case, you pass the call elements (the function
#'   to call and the arguments to call it with) separately.
#'
#' See section below for the difference between `call2()` and the base
#' constructors.
#'
#'
#' @param .fn Function to call. Must be a callable object: a string,
#'   symbol, call, or a function.
#' @param ... Arguments to the call either in or out of a list. These dots
#'   support [tidy dots][tidy-dots] features. Empty arguments are preserved.
#' @param .ns Namespace with which to prefix `.fn`. Must be a string
#'   or symbol.
#'
#'
#' @section Difference with base constructors:
#'
#' `call2()` is more flexible and convenient than `base::call()`:
#'
#' * The function to call can be a string or a [callable][is_callable]
#'   object: a symbol, another call (e.g. a `$` or `[[` call), or a
#'   function to inline. `base::call()` only supports strings and you
#'   need to use `base::as.call()` to construct a call with a callable
#'   object.
#'
#'   ```
#'   call2(list, 1, 2)
#'
#'   as.call(list(list, 1, 2))
#'   ```
#'
#' * The `.ns` argument is convenient for creating namespaced calls.
#'
#'   ```
#'   call2("list", 1, 2, .ns = "base")
#'
#'   ns_call <- as.call(list(as.name("::"), as.name("list"), as.name("base")))
#'   as.call(list(ns_call, 1, 2))
#'   ```
#'
#' * `call2()` has [tidy dots][list2] support and you can splice lists
#'   of arguments with `!!!`. With base R, you need to use `as.call()`
#'   instead of `call()` if the arguments are in a list.
#'
#'   ```
#'   args <- list(na.rm = TRUE, trim = 0)
#'
#'   call2("mean", 1:10, !!!args)
#'
#'   as.call(c(list(as.name("mean"), 1:10), args))
#'   ```
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
#'
#' @seealso call_modify
#' @examples
#' # fn can either be a string, a symbol or a call
#' call2("f", a = 1)
#' call2(quote(f), a = 1)
#' call2(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' call2(quote(f), a = 1, b = 2)
#' call2(quote(f), !!!list(a = 1, b = 2))
#'
#' # Creating namespaced calls is easy:
#' call2("fun", arg = quote(baz), .ns = "mypkg")
#'
#' # Empty arguments are preserved:
#' call2("[", quote(x), , drop = )
#' @export
call2 <- function(.fn, ..., .ns = NULL) {
  .External2(rlang_call2_external, .fn, .ns)
}
#' Create pairlists with splicing support
#'
#' This pairlist constructor supports [tidy dots][tidy-dots] features
#' like `!!!`. Use it to manually create argument lists for calls or
#' parameter lists for functions.
#'
#' @param ... Arguments stored in the pairlist. Empty arguments are
#'   preserved.
#'
#' @export
#' @examples
#' # Unlike `exprs()`, `pairlist2()` evaluates its arguments.
#' new_function(pairlist2(x = 1, y = 3 * 6), quote(x * y))
#' new_function(exprs(x = 1, y = 3 * 6), quote(x * y))
#'
#' # It preserves missing arguments, which is useful for creating
#' # parameters without defaults:
#' new_function(pairlist2(x = , y = 3 * 6), quote(x * y))
pairlist2 <- function(...) {
  .Call(rlang_dots_pairlist,
    frame_env = environment(),
    named = FALSE,
    ignore_empty = "trailing",
    preserve_empty = TRUE,
    unquote_names = TRUE,
    homonyms = "keep",
    check_assign = FALSE
  )
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
#' @keywords internal
#' @export
#' @examples
#' # Symbolic objects and functions are callable:
#' is_callable(quote(foo))
#' is_callable(base::identity)
#'
#' # node_poke_car() lets you modify calls without any checking:
#' lang <- quote(foo(10))
#' node_poke_car(lang, current_env())
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
#'
#'   Can be a character vector of namespaces, in which case the call
#'   has to match at least one of them, otherwise `is_call()` returns
#'   `FALSE`.
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
#' # You can supply multiple namespaces:
#' is_call(ns_expr, "list", ns = c("utils", "base"))
#' is_call(ns_expr, "list", ns = c("utils", "stats"))
#'
#' # If one of them is "", unnamespaced calls will match as well:
#' is_call(quote(list()), "list", ns = "base")
#' is_call(quote(list()), "list", ns = c("base", ""))
#' is_call(quote(base::list()), "list", ns = c("base", ""))
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
    good_ns <- FALSE

    for (elt in ns) {
      if (identical(elt, "") && !is_namespaced_call(x, private = FALSE)) {
        good_ns <- TRUE
        break
      } else if (is_namespaced_call(x, elt, private = FALSE)) {
        good_ns <- TRUE
        break
      }
    }

    if (!good_ns) {
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

# Until `is_call()` is fixed
is_call2 <- function(x, ...) {
  if (is_quosure(x)) {
    FALSE
  } else {
    rlang::is_call(x, ...)
  }
}

#' How does a call print at the console?
#'
#' @description
#'
#' `call_print_type()` returns the way a call is deparsed and printed
#' at the console. This is useful when generating documents based on R
#' code. The types of calls are:
#'
#' * `"prefix"` for calls like `foo()`, unary operators, `-1`, and
#'   operators with more than 2 arguments like \code{`+`(1, 2, 3)}
#'   (the latter can be obtained by building calls manually).
#'
#' * `"infix"` for operators like `1 + 2` or `foo$bar`.
#'
#' * `"special"` for function definitions, control-flow calls like
#'   `if` or `for`, and subscripting calls like `foo[]` and `foo[[]]`.
#'
#'
#' @param call A quoted function call. An error is raised if not a call.
#' @examples
#' call_print_type(quote(foo(bar)))
#' call_print_type(quote(foo[[bar]]))
#' call_print_type(quote(+foo))
#' call_print_type(quote(function() foo))
#'
#' # When an operator call has an artificial number of arguments, R
#' # often prints it in prefix form:
#' call <- call("+", 1, 2, 3)
#' call
#' call_print_type(call)
#'
#' # But not always:
#' call <- call("$", 1, 2, 3)
#' call
#' call_print_type(call)
#' @noRd
call_print_type <- function(call) {
  type <- call_print_fine_type(call)

  switch(type,
    call = "prefix",
    control = ,
    delim = ,
    subset = "special",
    type
  )
}
call_print_fine_type <- function(call) {
  if (!is_call(call)) {
    abort("`call` must be a call")
  }

  op <- which_operator(call)
  if (op == "") {
    return("call")
  }

  switch(op,
    `+unary` = ,
    `-unary` = ,
    `~unary` = ,
    `?unary` = ,
    `!` = ,
    `!!` = ,
    `!!!` =
      "prefix",
    `function` = ,
    `while` = ,
    `for` = ,
    `repeat` = ,
    `if` =
      "control",
    `(` = ,
    `{` =
      "delim",
    `[` = ,
    `[[` =
      "subset",
    # These operators always print in infix form even if they have
    # more arguments
    `<-` = ,
    `<<-` = ,
    `=` = ,
    `::` = ,
    `:::` = ,
    `$` = ,
    `@` =
      "infix",
    `+` = ,
    `-` = ,
    `?` = ,
    `~` = ,
    `:=` = ,
    `|` = ,
    `||` = ,
    `&` = ,
    `&&` = ,
    `>` = ,
    `>=` = ,
    `<` = ,
    `<=` = ,
    `==` = ,
    `!=` = ,
    `*` = ,
    `/` = ,
    `%%` = ,
    `special` = ,
    `:` = ,
    `^` =
      if (length(node_cdr(call)) == 2) {
        "infix"
      } else {
        "call"
      }
  )
}


#' Modify the arguments of a call
#'
#' If you are working with a user-supplied call, make sure the
#' arguments are standardised with [call_standardise()] before
#' modifying the call.
#'
#'
#' @inheritParams tidy-dots
#' @param .call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression.
#' @param ... Named or unnamed expressions (constants, names or calls)
#'   used to modify the call. Use [zap()] to remove arguments. These
#'   dots support [tidy dots][tidy-dots] features. Empty arguments are
#'   allowed and preserved.
#' @param .standardise,.env Soft-deprecated as of rlang 0.3.0. Please
#'   call [call_standardise()] manually.
#'
#' @section Life cycle:
#'
#' * The `.standardise` argument is deprecated as of rlang 0.3.0.
#'
#' * In rlang 0.2.0, `lang_modify()` was deprecated and renamed to
#'   `call_modify()`. See lifecycle section in [call2()] for more about
#'   this change.
#'
#' @return A quosure if `.call` is a quosure, a call otherwise.
#' @export
#' @examples
#' call <- quote(mean(x, na.rm = TRUE))
#'
#' # Modify an existing argument
#' call_modify(call, na.rm = FALSE)
#' call_modify(call, x = quote(y))
#'
#' # Remove an argument
#' call_modify(call, na.rm = zap())
#'
#' # Add a new argument
#' call_modify(call, trim = 0.1)
#'
#' # Add an explicit missing argument:
#' call_modify(call, na.rm = )
#'
#' # Supply a list of new arguments with `!!!`
#' newargs <- list(na.rm = NULL, trim = 0.1)
#' call <- call_modify(call, !!!newargs)
#' call
#'
#' # Remove multiple arguments by splicing zaps:
#' newargs <- rep_named(c("na.rm", "trim"), list(zap()))
#' call <- call_modify(call, !!!newargs)
#' call
#'
#'
#' # Modify the `...` arguments as if it were a named argument:
#' call <- call_modify(call, ... = )
#' call
#'
#' call <- call_modify(call, ... = zap())
#' call
#'
#'
#' # When you're working with a user-supplied call, standardise it
#' # beforehand because it might contain unmatched arguments:
#' user_call <- quote(matrix(x, nc = 3))
#' call_modify(user_call, ncol = 1)
#'
#' # Standardising applies the usual argument matching rules:
#' user_call <- call_standardise(user_call)
#' user_call
#' call_modify(user_call, ncol = 1)
#'
#'
#' # You can also modify quosures inplace:
#' f <- quo(matrix(bar))
#' call_modify(f, quote(foo))
#'
#'
#' # By default, arguments with the same name are kept. This has
#' # subtle implications, for instance you can move an argument to
#' # last position by removing it and remapping it:
#' call <- quote(foo(bar = , baz))
#' call_modify(call, bar = NULL, bar = missing_arg())
#'
#' # You can also choose to keep only the first or last homonym
#' # arguments:
#' args <-  list(bar = NULL, bar = missing_arg())
#' call_modify(call, !!!args, .homonyms = "first")
#' call_modify(call, !!!args, .homonyms = "last")
call_modify <- function(.call,
                        ...,
                        .homonyms = c("keep", "first", "last", "error"),
                        .standardise = NULL,
                        .env = caller_env()) {
  args <- dots_list(..., .preserve_empty = TRUE, .homonyms = .homonyms)
  expr <- get_expr(.call)

  if (!is_null(.standardise)) {
    warn_deprecated(paste_line(
      "`.standardise` is deprecated as of rlang 0.3.0.",
      "Please use `call_standardise()` prior to calling `call_modify()`."
    ))
    if (.standardise) {
      expr <- get_expr(call_standardise(.call, env = .env))
    }
  }

  if (!is_call(expr)) {
    abort_call_input_type(".call")
  }

  expr <- duplicate(expr, shallow = TRUE)

  # Discard "" names
  nms <- names2(args)
  named <- have_name(args)
  named_args <- args[named]

  for (i in seq_along(args)) {
    tag <- sym(nms[[i]])
    arg <- args[[i]]

    if (is_missing(tag)) {
      if (is_zap(arg)) {
        abort("Zap sentinels can't be unnamed")
      }
      node_append(expr, new_node(arg))
      next
    }

    if (identical(tag, dots_sym)) {
      # Unwrap empty quosures. Useful for passing captured arguments
      # to `call_modify()`.
      if (identical(maybe_missing(arg), quo())) {
        arg <- missing_arg()
      }
      if (!is_missing(arg) && !is_zap(arg)) {
        abort("`...` arguments must be `zap()` or empty")
      }
      node_accessor <- node_car
    } else {
      node_accessor <- node_tag
    }

    prev <- expr
    node <- node_cdr(expr)

    while (!is_null(node)) {
      if (identical(node_accessor(node), tag)) {
        # Remove argument from the list if a zap sentinel
        if (is_zap(maybe_missing(arg))) {
          node <- node_cdr(node)
          node_poke_cdr(prev, node)
          next
        }

        # If `...` it can only be missing at this point, which means
        # we keep it in the argument list as is
        if (!identical(tag, dots_sym)) {
          node_poke_car(node, maybe_missing(arg))
        }

        break
      }

      prev <- node
      node <- node_cdr(node)
    }

    if (is_null(node) && !is_zap(maybe_missing(arg))) {
      if (identical(tag, dots_sym)) {
        node <- new_node(dots_sym, NULL)
        node_poke_cdr(prev, node)
      } else {
        node <- new_node(maybe_missing(arg), NULL)
        node_poke_tag(node, tag)
        node_poke_cdr(prev, node)
      }
    }
  }

  set_expr(.call, expr)
}

abort_call_input_type <- function(arg) {
  abort(sprintf("`%s` must be a quoted call", arg))
}

#' Standardise a call
#'
#' This is essentially equivalent to [base::match.call()], but with
#' experimental handling of primitive functions.
#'
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_standardise()` was deprecated and renamed to
#' `call_standardise()`. See lifecycle section in [call2()] for more
#' about this change.
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
  if (!is_call(expr)) {
    abort_call_input_type("call")
  }

  if (is_frame(call)) {
    fn <- call$fn
  } else {
    # The call name might be a literal, not necessarily a symbol
    env <- get_env(call, env)
    fn <- eval_bare(node_car(expr), env)
  }

  if (is_primitive(fn)) {
    call
  } else {
    matched <- match.call(fn, expr)
    set_expr(call, matched)
  }
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
#' In rlang 0.2.0, `lang_fn()` was deprecated and renamed to
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
    abort_call_input_type("call")
  }

  switch(call_type(expr),
    recursive = abort("`call` does not call a named or inlined function"),
    inlined = node_car(expr),
    named = ,
    namespaced = ,
    eval_bare(node_car(expr), env)
  )
}

#' Extract function name or namespaced of a call
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_name()` was deprecated and renamed to
#' `call_name()`. See lifecycle section in [call2()] for more about
#' this change.
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
#' # Namespaced calls are correctly handled:
#' call_name(~base::matrix(baz))
#'
#' # Anonymous and subsetted functions return NULL:
#' call_name(quote(foo$bar()))
#' call_name(quote(foo[[bar]]()))
#' call_name(quote(foo()()))
#'
#' # Extract namespace of a call with call_ns():
#' call_ns(quote(base::bar()))
#'
#' # If not namespaced, call_ns() returns NULL:
#' call_ns(quote(bar()))
call_name <- function(call) {
  if (is_quosure(call) || is_formula(call)) {
    call <- get_expr(call)
  }

  # FIXME: Disabled for the 0.3.1 release
  # if (!is_call(call) || is_call(call, c("::", ":::"))) {

  if (!is_call(call)) {
    abort_call_input_type("call")
  }

  switch(call_type(call),
    named = as_string(node_car(call)),
    namespaced = as_string(node_cadr(node_cdar(call))),
    NULL
  )
}
#' @rdname call_name
#' @export
call_ns <- function(call) {
  if (is_quosure(call) || is_formula(call)) {
    call <- get_expr(call)
  }

  if (!is_call(call)) {
    abort_call_input_type("call")
  }

  head <- node_car(call)
  if (is_call(head, c("::", ":::"))) {
    as_string(node_cadr(head))
  } else {
    NULL
  }
}

#' Extract arguments from a call
#'
#' @section Life cycle:
#'
#' In rlang 0.2.0, `lang_args()` and `lang_args_names()` were
#' deprecated and renamed to `call_args()` and `call_args_names()`.
#' See lifecycle section in [call2()] for more about this change.
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
  if (!is_call(call)) {
    abort_call_input_type("call")
  }

  args <- as.list(call[-1])
  set_names((args), names2(args))
}
#' @rdname call_args
#' @export
call_args_names <- function(call) {
  call <- get_expr(call)
  if (!is_call(call)) {
    abort_call_input_type("call")
  }
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

call_type <- function(x) {
  x <- get_expr(x)
  stopifnot(typeof(x) == "language")

  type <- typeof(node_car(x))
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(node_car(x))) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    abort("corrupt language object")
  }
}
