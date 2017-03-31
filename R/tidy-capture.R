#' Capture an expression and evaluation environment.
#'
#' A powerful feature in R is the ability of capturing the expression
#' supplied in a function call. Once captured, this expression can be
#' inspected and evaluated, possibly within an altered scope
#' environment (for instance a scope where all elements of a data
#' frame are directly accessible). The tricky part of capturing an
#' expression for rescoping purposes is to keep the original
#' evaluation environment accessible so that other variables defined
#' at the call site are available when the expression gets
#' evaluated. It is thus necessary to capture not only the R
#' expression supplied as argument in a function call, but also the
#' evaluation environment of the call site. `enquo()` and
#' `dots_quos()` make it easy to record this information within
#' formulas.
#'
#' @section Non-standard evaluation:
#'
#'   The two main purposes for capturing arguments are labelling and
#'   rescoping. With labelling, the normal R evaluation rules are kept
#'   unchanged. The captured expression is only used to provide
#'   default names (or labels) to output elements, such as column
#'   names for a function that manipulates data frames or axis labels
#'   for a function that creates graphics. In the case of rescoping
#'   however, evaluation rules are altered. Functions that capture and
#'   rescope arguments are said to use non-standard evaluation, or
#'   NSE. The approach we recommend in rlang is to always create two
#'   versions of such functions: a NSE version that captures
#'   arguments, and another that work with captured arguments with
#'   standard evaluation rules (see `decorate_nse()`). Providing a
#'   standard evaluation version simplifies programming tasks. Also,
#'   it makes it possible to forward named arguments across function
#'   calls (see below). See `vignette("nse")` for more information on
#'   NSE.
#'
#'   In addition, note that `enquo()` always interpolates its
#'   input to facilitate programming with NSE functions. See
#'   [expr_interp()] and [quo()].
#'
#' @section Forwarding arguments:
#'
#'   You have to be a bit careful when you pass arguments between
#'   introspective functions as only the most immediate call site is
#'   captured. For this reason, named arguments should be captured by
#'   an NSE function at the outermost level, and then passed around to
#'   SE versions that handle pre-captured arguments. See
#'   [arg_inspect()] for another approach to introspecting arguments
#'   with which it is possible to capture expressions at the outermost
#'   call site. This approach may be harder to reason about and has
#'   some limitations.
#'
#'   Dots are different from named arguments in that they are
#'   implicitly forwarded. Forwarding dots does not create a new call
#'   site. The expression is passed on as is. That's why you can
#'   easily capture them with [base::substitute()]. By the same token,
#'   you don't need to capture dots before passing them along in
#'   another introspective function. You do need to be a bit careful
#'   when you rescope expressions captured from dots because those
#'   expressions were not necessarily supplied in the last call
#'   frame. In general, the call site of argument passed through dots
#'   can be anywhere between the current and global frames. For this
#'   reason, it is recommended to always use `dots_quos()` rather
#'   than `substitute()` and `caller_env()` or `parent.frame()`, since
#'   the former will encode the appropriate evaluation environments
#'   within the formulas.
#'
#' @param x,... Arguments to capture.
#' @export
#' @return `enquo()` returns a formula; see also
#'   `dots_quos()` for "capturing" dots as a list of formulas.
#' @seealso [quos()][quosures] for capturing dots, [expr_label()] and
#'   [expr_text()] for capturing labelling information.
#' @examples
#' # enquo() returns a formula:
#' fn <- function(foo) enquo(foo)
#' fn(a + b)
#'
#' # Capturing an argument only works for the most direct call:
#' g <- function(bar) fn(bar)
#' g(a + b)
enquo <- function(x) {
  if (missing(x)) {
    return(new_quosure(missing_arg(), empty_env()))
  }

  capture <- new_language(captureArg, substitute(x))
  arg <- eval_bare(capture, caller_env())
  expr <- .Call(rlang_interp, arg$expr, arg$env, TRUE)
  forward_quosure(expr, arg$env)
}
forward_quosure <- function(expr, env) {
  if (quo_is_missing(expr)) {
    expr
  } else if (is_definition(expr)) {
    as_quosureish(expr, env)
  } else if (is_formula(expr)) {
    as_quosure(expr, env)
  } else if (is_symbolic(expr)) {
    new_quosure(expr, env)
  } else {
    as_quosure(expr, empty_env())
  }
}
#' @rdname enquo
#' @export
#' @examples
#'
#' # enexpr() returns a bare expression instead of a quosure:
#' fn <- function(x) enexpr(x)
#' fn(foo(bar))
#'
#' # It supports unquoting as well:
#' fn(foo(bar, !! letters[1:3]))
enexpr <- function(x) {
  if (missing(x)) {
    return(missing_arg())
  }

  capture <- new_language(captureArg, substitute(x))
  arg <- eval_bare(capture, caller_env())
  .Call(rlang_interp, arg$expr, arg$env, TRUE)
}

dots_enquose <- function(...) {
  info <- captureDots()
  dots <- map(info, dot_interp)

  # Flatten possibly spliced dots
  dots <- unlist(dots, FALSE) %||% list()

  map(dots, dot_enquose)
}
dot_interp <- function(dot, quosured = TRUE) {
  if (is_missing(dot$expr)) {
    return(list(dot))
  }
  env <- dot$env
  expr <- dot$expr

  # Allow unquote-splice in dots
  if (is_splice(expr)) {
    dots <- call("alist", expr)
    dots <- .Call(rlang_interp, dots, env, quosured)
    dots <- eval_bare(dots)
    map(dots, function(expr) list(expr = expr, env = env))
  } else {
    expr <- .Call(rlang_interp, expr, env, quosured)
    list(list(expr = expr, env = env))
  }
}
dot_enquose <- function(dot) {
  if (is_missing(dot$expr)) {
    new_quosure(missing_arg(), empty_env())
  } else {
    forward_quosure(dot$expr, dot$env)
  }
}

is_bang <- function(expr) {
  is_lang(expr) && identical(node_car(expr), quote(`!`))
}
is_splice <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  if (identical(node_car(expr), quote(UQS)) || identical(node_car(expr), quote(rlang::UQS))) {
    return(TRUE)
  }

  if (is_bang(expr) && is_bang(node_cadr(expr)) && is_bang(node_cadr(node_cadr(expr)))) {
    return(TRUE)
  }

  FALSE
}

dots_interp_lhs <- function(dots) {
  nms <- names2(dots)
  defs <- map_lgl(dots, is_definition)

  for (i in which(defs)) {
    dot <- dot_interp_lhs(nms[[i]], dots[[i]])
    dots[[i]] <- dot$expr

    if (!is_null(dot$name)) {
      nms[[i]] <- dot$name
    }
  }

  set_names(dots, nms)
}
dot_interp_lhs <- function(name, dot) {
  if (!is_null(name) && name != "") {
    warn("name ignored because a LHS was supplied")
  }

  rhs <- new_quosure(f_rhs(dot), env = f_env(dot))
  lhs <- .Call(rlang_interp, f_lhs(dot), f_env(dot), TRUE)

  if (is_symbol(lhs)) {
    lhs <- as_string(lhs)
  } else if (!is_string(lhs)) {
    abort("LHS must be a name or string")
  }

  list(name = lhs, expr = rhs)
}
