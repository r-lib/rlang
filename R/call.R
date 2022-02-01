#' Create a call
#'
#' @description
#'
#' Quoted function calls are one of the two types of
#' [symbolic][is_symbolic] objects in R. They represent the action of
#' calling a function, possibly with arguments. There are two ways of
#' creating a quoted call:
#'
#' * By [quoting][nse-defuse] it. Quoting prevents functions from being
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
#' @param ... <[dynamic][dyn-dots]> Arguments for the function
#'   call. Empty arguments are preserved.
#' @param .ns Namespace with which to prefix `.fn`. Must be a string
#'   or symbol.
#'
#'
#' @section Difference with base constructors:
#'
#' `call2()` is more flexible than `base::call()`:
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
#'   # Equivalent to
#'   ns_call <- call("::", as.symbol("list"), as.symbol("base"))
#'   as.call(list(ns_call, 1, 2))
#'   ```
#'
#' * `call2()` has [dynamic dots][list2] support. You can splice lists
#'   of arguments with `!!!` or unquote an argument name with glue
#'   syntax.
#'
#'   ```
#'   args <- list(na.rm = TRUE, trim = 0)
#'
#'   call2("mean", 1:10, !!!args)
#'
#'   # Equivalent to
#'   as.call(c(list(as.symbol("mean"), 1:10), args))
#'   ```
#'
#'
#' @section Caveats of inlining objects in calls:
#'
#' `call2()` makes it possible to inline objects in calls, both in
#' function and argument positions. Inlining an object or a function
#' has the advantage that the correct object is used in all
#' environments. If all components of the code are inlined, you can
#' even evaluate in the [empty environment][empty_env].
#'
#' However inlining also has drawbacks. It can cause issues with NSE
#' functions that expect symbolic arguments. The objects may also leak
#' in representations of the call stack, such as [traceback()].
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
  .External2(ffi_call2, .fn, .ns)
}
#' Collect dynamic dots in a pairlist
#'
#' This pairlist constructor uses [dynamic dots][dyn-dots]. Use
#' it to manually create argument lists for calls or parameter lists
#' for functions.
#'
#' @param ... <[dynamic][dyn-dots]> Arguments stored in the
#'   pairlist. Empty arguments are preserved.
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
  .Call(ffi_dots_pairlist,
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
#'
#' @param x An object to test. Formulas and quosures are treated
#'   literally.
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
  .Call(ffi_is_call, x, name, n, ns)
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
#' @param call A quoted function call. An error is raised if not a call.
#'
#' @section Finer print types:
#'
#' `call_print_fine_type()` is a lower level version with the following
#' differences:
#'
#' * The `"special"` calls are categorised as `"control"` (`if`,
#'   `for`, `while`, `repeat`, `function`), `"delim"` (`(` and `{`),
#'   and `"subset"` (`[` and `[[`).
#'
#' * The `"prefixed"` calls are categorised as `"prefix"` (`+`, `-`,
#'   ...) and `"calls"`.
#'
#'
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
  check_call(call)

  type <- call_print_fine_type(call)
  switch(
    type,
    call = "prefix",
    control = ,
    delim = ,
    subset = "special",
    type
  )
}
call_print_fine_type <- function(call) {
  check_call(call)

  op <- call_parse_type(call)
  if (op == "") {
    return("call")
  }

  switch(
    op,
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

#' Is object a call to infix operator?
#' @param x An object.
#' @return `FALSE` if not a call or not a call to an infix
#'   operator. `TRUE` otherwise.
#' @noRd
#' @examples
#' is_call_infix(quote(-1))
#' is_call_infix(quote(1 - 2))
#' is_call_infix(quote(1 %-% 2))
#' is_call_infix(quote(a@b))
is_call_infix <- function(x) {
  switch(
    call_parse_type(x),
    `<-` = ,
    `<<-` = ,
    `=` = ,
    `::` = ,
    `:::` = ,
    `$` = ,
    `@` = ,
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
    `^` = TRUE,
    FALSE
  )
}

#' What is the parser type of a call?
#' @return A string, one of:
#' - `""`
#' - `"break"`
#' - `"next"`
#' - `"while"`
#' - `"for"`
#' - `"repeat"`
#' - `"if"`
#' - `"function"`
#' - `"?"`
#' - `"?unary"`
#' - `"<-"`
#' - `"<<-"`
#' - `"="`
#' - `":="`
#' - `"~"`
#' - `"~unary"`
#' - `"|"`
#' - `"||"`
#' - `"&"`
#' - `"&&"`
#' - `"!"`
#' - `"!!!"`
#' - `">"`
#' - `">="`
#' - `"<"`
#' - `"<="`
#' - `"=="`
#' - `"!="`
#' - `"+"`
#' - `"-"`
#' - `"*"`
#' - `"/"`
#' - `"%%"`
#' - `"special"`
#' - `":"`
#' - `"!!"`
#' - `"+unary"`
#' - `"-unary"`
#' - `"^"`
#' - `"$"`
#' - `"@"`
#' - `"::"`
#' - `":::"`
#' - `"("`
#' - `"["`
#' - `"[["`
#' - `"{"`
#' @keywords internal
#' @noRd
call_parse_type <- function(call) {
  .Call(ffi_which_operator, call)
}
call_has_precedence <- function(call, parent_call, side = NULL) {
  side <- switch(
    side %||% "none",
    lhs = -1L,
    none = 0L,
    rhs = 1L,
    abort("Unexpected `side` value in `call_has_precendence()`.")
  )
  .Call(ffi_call_has_precedence, call, parent_call, side)
}


#' Modify the arguments of a call
#'
#' If you are working with a user-supplied call, make sure the
#' arguments are standardised with [call_match()] before
#' modifying the call.
#'
#' @inheritParams dots_list
#' @param .call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression.
#' @param ... <[dynamic][dyn-dots]> Named or unnamed expressions
#'   (constants, names or calls) used to modify the call. Use [zap()]
#'   to remove arguments. Empty arguments are preserved.
#' @param .standardise,.env Deprecated as of rlang 0.3.0. Please
#'   call [call_match()] manually.
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
#' # beforehand in case it includes unmatched arguments:
#' user_call <- quote(matrix(x, nc = 3))
#' call_modify(user_call, ncol = 1)
#'
#' # `call_match()` applies R's argument matching rules. Matching
#' # ensures you're modifying the intended argument.
#' user_call <- call_match(user_call, matrix)
#' user_call
#' call_modify(user_call, ncol = 1)
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
  check_call(expr, arg = ".call")

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

abort_call_input_type <- function(arg, call = caller_env()) {
  abort(
    sprintf("%s must be a quoted call.", format_arg(arg)),
    call = call
  )
}
abort_simple_call_input_type <- function(arg, fn, call = caller_env()) {
  msg <- c(
    sprintf("%s must be a simple call.", format_arg(arg)),
    i = "Calls to `::` or `:::` are not simple calls.",
    i = sprintf("See %s.", format_code("?is_call_simple"))
  )
  abort(msg, call = call)
}

#' Match supplied arguments to function definition
#'
#' @description
#' `call_match()` is like [match.call()] with these differences:
#'
#' - It supports matching missing argument to their defaults in the
#'   function definition.
#'
#' - It requires you to be a little more specific in some cases.
#'   Either all arguments are inferred from the call stack or none of
#'   them are (see the Inference section).
#'
#' @param call A call. The arguments will be matched to `fn`.
#' @param fn A function definition to match arguments to.
#' @param ... These dots must be empty.
#' @param defaults Whether to match missing arguments to their
#'   defaults.
#' @param dots_env An execution environment where to find dots. If
#'   supplied and dots exist in this environment, and if `call`
#'   includes `...`, the forwarded dots are matched to numbered dots
#'   (e.g. `..1`, `..2`, etc). By default this is set to the empty
#'   environment which means that `...` expands to nothing.
#' @param dots_expand If `FALSE`, arguments passed through `...` will
#'   not be spliced into `call`. Instead, they are gathered in a
#'   pairlist and assigned to an argument named `...`. Gathering dots
#'   arguments is useful if you need to separate them from the other
#'   named arguments.
#'
#'   Note that the resulting call is not meant to be evaluated since R
#'   does not support passing dots through a named argument, even if
#'   named `"..."`.
#'
#' @section Inference from the call stack:
#' When `call` is not supplied, it is inferred from the call stack
#' along with `fn` and `dots_env`.
#'
#' - `call` and `fn` are inferred from the calling environment:
#'   `sys.call(sys.parent())` and `sys.function(sys.parent())`.
#'
#' - `dots_env` is inferred from the caller of the calling
#'   environment: `caller_env(2)`.
#'
#' If `call` is supplied, then you must supply `fn` as well. Also
#' consider supplying `dots_env` as it is set to the empty environment
#' when not inferred.
#'
#' @examples
#' # `call_match()` supports matching missing arguments to their
#' # defaults
#' fn <- function(x = "default") fn
#' call_match(quote(fn()), fn)
#' call_match(quote(fn()), fn, defaults = TRUE)
#' @export
call_match <- function(call = NULL,
                       fn = NULL,
                       ...,
                       defaults = FALSE,
                       dots_env = NULL,
                       dots_expand = TRUE) {
  check_dots_empty0(...)

  if (is_null(call)) {
    call <- sys.call(sys.parent())
    fn <- fn %||% sys.function(sys.parent())
    dots_env <- dots_env %||% caller_env(2)
  } else {
    dots_env <- dots_env %||% empty_env()
  }

  if (is_null(fn)) {
    abort("`fn` must be supplied.")
  }
  if (!is_environment(dots_env)) {
    abort("`dots_env` must be an environment.")
  }

  if (is_primitive(fn)) {
    return(call)
  }

  # Don't expand dots before matching defaults to make it easier to
  # sort the arguments by formals
  call <- match.call(fn, call, expand.dots = FALSE, envir = dots_env)

  if (defaults) {
    fmls <- fn_fmls(fn)
    names <- names(fmls)
    missing <- !names %in% names(call)

    args <- c(as.list(call[-1]), fmls[missing])
    args <- args[names]
    call <- call2(call[[1]], !!!args)
  }

  if (is_missing(call$...)) {
    call$... <- NULL
    return(call)
  }

  if (!dots_expand) {
    return(call)
  }

  i <- match("...", names(call))
  if (is_na(i)) {
    return(call)
  }

  call <- as.list(call)
  as.call(c(
    call[seq2(1, i - 1)],
    call$...,
    call[seq2(i + 1, length(call))]
  ))
}

#' Extract function name or namespace of a call
#'
#' @description
#' `call_name()` and `call_ns()` extract the function name or
#' namespace of _simple_ calls as a string. They return `NULL` for
#' complex calls.
#'
#' * Simple calls: `foo()`, `bar::foo()`.
#' * Complex calls: `foo()()`, `bar::foo`, `foo$bar()`, `(function() NULL)()`.
#'
#' The `is_call_simple()` predicate helps you determine whether a call
#' is simple. There are two invariants you can count on:
#'
#' 1. If `is_call_simple(x)` returns `TRUE`, `call_name(x)` returns a
#'    string. Otherwise it returns `NULL`.
#'
#' 2. If `is_call_simple(x, ns = TRUE)` returns `TRUE`, `call_ns()`
#'    returns a string. Otherwise it returns `NULL`.
#'
#' @param call A defused call.
#' @return The function name or namespace as a string, or `NULL` if
#'   the call is not named or namespaced.
#'
#' @examples
#' # Is the function named?
#' is_call_simple(quote(foo()))
#' is_call_simple(quote(foo[[1]]()))
#'
#' # Is the function namespaced?
#' is_call_simple(quote(list()), ns = TRUE)
#' is_call_simple(quote(base::list()), ns = TRUE)
#'
#' # Extract the function name from quoted calls:
#' call_name(quote(foo(bar)))
#' call_name(quo(foo(bar)))
#'
#' # Namespaced calls are correctly handled:
#' call_name(quote(base::matrix(baz)))
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
#' @export
call_name <- function(call) {
  check_required(call)

  if (is_quosure(call) || is_formula(call)) {
    call <- get_expr(call)
  }
  check_call(call)

  if (is_call(call, c("::", ":::"))) {
    return(NULL)
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
  check_required(call)

  if (is_quosure(call) || is_formula(call)) {
    call <- get_expr(call)
  }
  check_call(call)

  if (!is_call(call)) {
    abort_call_input_type("call")
  }

  head <- call[[1]]
  if (is_call(head, c("::", ":::"))) {
    as_string(head[[2]])
  } else {
    NULL
  }
}
#' @rdname call_name
#' @param x An object to test.
#' @param ns Whether call is namespaced. If `NULL`, `is_call_simple()`
#'   is insensitive to namespaces. If `TRUE`, `is_call_simple()`
#'   detects namespaced calls. If `FALSE`, it detects unnamespaced
#'   calls.
#' @export
is_call_simple <- function(x, ns = NULL) {
  check_required(x)

  # For compatibility with `call_name()` and `call_ns()`
  if (is_quosure(x) || is_formula(x)) {
    x <- get_expr(x)
  }

  if (!is_call(maybe_missing(x))) {
    return(FALSE)
  }

  if (is_call(x, c("::", ":::"))) {
    return(FALSE)
  }

  head <- x[[1]]
  namespaced <- is_call(head, c("::", ":::"))

  if (!is_null(ns) && !identical(namespaced, ns)) {
    return(FALSE)
  }

  namespaced || is_symbol(head)
}

#' Extract arguments from a call
#'
#' @inheritParams call_name
#' @return A named list of arguments.
#' @seealso [fn_fmls()] and [fn_fmls_names()]
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
#' @export
call_args <- function(call) {
  check_required(call)

  if (is_quosure(call) || is_formula(call)) {
    call <- get_expr(call)
  }
  check_call(call)

  args <- as.list(call[-1])
  set_names((args), names2(args))
}
#' @rdname call_args
#' @export
call_args_names <- function(call) {
  check_required(call)

  if (is_quosure(call) || is_formula(call)) {
    call <- get_expr(call)
  }
  check_call(call)

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

call_zap_inline <- function(x) {
  .Call(ffi_call_zap_inline, x)
}

# Called from C
call_type_sum <- function(x) {
  sym(sprintf("<%s>", rlang_type_sum(x)))
}
