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
#' evaluation environment of the call site. \code{tidy_capture()} and
#' \code{tidy_quotes()} make it easy to record this information
#' within formulas.
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
#'   standard evaluation rules (see \code{decorate_nse()}). Providing
#'   a standard evaluation version simplifies programming tasks. Also,
#'   it makes it possible to forward named arguments across function
#'   calls (see below). See \code{vignette("nse")} for more
#'   information on NSE.
#'
#'   In addition, note that \code{tidy_capture()} always interpolates
#'   its input to facilitate programming with NSE functions. See
#'   \code{\link{tidy_interp}()} and \code{\link{tidy_quote}()}.
#'
#' @section Forwarding arguments:
#'
#'   You have to be a bit careful when you pass arguments between
#'   introspective functions as only the most immediate call site is
#'   captured. For this reason, named arguments should be captured by
#'   an NSE function at the outermost level, and then passed around to
#'   SE versions that handle pre-captured arguments. See
#'   \code{\link{arg_inspect}()} for another approach to introspecting
#'   arguments with which it is possible to capture expressions at the
#'   outermost call site. This approach may be harder to reason about
#'   and has some limitations.
#'
#'   Dots are different from named arguments in that they are
#'   implicitly forwarded. Forwarding dots does not create a new call
#'   site. The expression is passed on as is. That's why you can
#'   easily capture them with \code{\link[base]{substitute}()}. By the
#'   same token, you don't need to capture dots before passing them
#'   along in another introspective function. You do need to be a bit
#'   careful when you rescope expressions captured from dots because
#'   those expressions were not necessarily supplied in the last call
#'   frame. In general, the call site of argument passed through dots
#'   can be anywhere between the current and global frames. For this
#'   reason, it is recommended to always use \code{tidy_quotes()}
#'   rather than \code{substitute()} and \code{caller_env()} or
#'   \code{parent.frame()}, since the former will encode the
#'   appropriate evaluation environments within the formulas.
#'
#' @param x,... Arguments to capture.
#' @export
#' @return \code{tidy_capture()} returns a formula; see also
#'   \code{tidy_quotes()} for "capturing" dots as a list of formulas.
#' @seealso \code{\link{tidy_quotes}()} for capturing dots,
#'   \code{\link{expr_label}()} and \code{\link{expr_text}()} for
#'   capturing labelling information.
#' @examples
#' # tidy_capture() returns a formula:
#' fn <- function(foo) tidy_capture(foo)
#' fn(a + b)
#'
#' # Capturing an argument only works for the most direct call:
#' g <- function(bar) fn(bar)
#' g(a + b)
tidy_capture <- function(x) {
  capture <- lang(captureArg, substitute(x))
  arg <- expr_eval(capture, caller_env())
  expr <- .Call(rlang_interp, arg$expr, arg$env)
  new_tidy_quote(expr, arg$env)
}

tidy_capture_dots <- function(..., .named) {
  info <- captureDots()
  dots <- map(info, dot_f)

  # Flatten possibly spliced dots
  dots <- unlist(dots, FALSE)

  if (.named) {
    if (is_true(.named)) {
      width <- formals(expr_text)$width
    } else if (is_scalar_integerish(.named)) {
      width <- .named
    } else {
      abort("`.named` must be a scalar logical or a numeric")
    }
    have_names <- have_names(dots)
    names(dots)[!have_names] <- map_chr(dots[!have_names], f_text, width = width)
  }

  dots
}
dot_f <- function(dot) {
  env <- dot$env
  expr <- dot$expr

  # Allow unquote-splice in dots
  if (is_splice(expr)) {
    dots <- call("alist", expr)
    dots <- .Call(rlang_interp, dots, env)
    dots <- expr_eval(dots)
    map(dots, as_tidy_quote, env)
  } else {
    expr <- .Call(rlang_interp, expr, env)
    list(new_tidy_quote(expr, env))
  }
}

is_bang <- function(expr) {
  is.call(expr) && identical(car(expr), quote(`!`))
}
is_splice <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  if (identical(car(expr), quote(UQS)) || identical(car(expr), quote(rlang::UQS))) {
    return(TRUE)
  }

  if (is_bang(expr) && is_bang(cadr(expr)) && is_bang(cadr(cadr(expr)))) {
    return(TRUE)
  }

  FALSE
}

dots_interp_lhs <- function(dots) {
  orig_names <- names(dots)
  names <- names2(dots)
  interpolated <- FALSE

  for (i in seq_along(dots)) {
    dot <- dot_interp_lhs(orig_names[[i]], dots[[i]])
    dots[[i]] <- dot$dot

    # Make sure unnamed dots remain unnamed
    if (!is_null(dot$name)) {
      interpolated <- TRUE
      names[[i]] <- dot$name
    }
  }

  if (interpolated) {
    names(dots) <- names
  }

  dots
}
dot_interp_lhs <- function(name, dot) {
  if (!is_formula(dot) || !is_definition(f_rhs(dot))) {
    return(list(name = name, dot = dot))
  }

  if (!is_null(name) && name != "") {
    warn("name ignored because a LHS was supplied")
  }

  rhs <- new_tidy_quote(f_rhs(f_rhs(dot)), env = f_env(dot))
  lhs <- .Call(rlang_interp, f_lhs(f_rhs(dot)), f_env(dot))

  if (is_symbol(lhs)) {
    lhs <- as.character(lhs)
  } else if (!is_scalar_character(lhs)) {
    abort("LHS must be a name or string")
  }

  list(name = lhs, dot = rhs)
}
