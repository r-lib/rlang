#' Introspect an argument
#'
#' \code{arg_info()} and \code{arg_info_()} are the workhorses of all
#' functions providing information about the evaluation context of an
#' argument (e.g., \code{\link{arg_expr}()}, \code{\link{arg_env}()},
#' etc). These functions climb the call stack to find where an
#' argument was first supplied, with which expression, in which
#' evaluation environment.
#'
#' \code{arg_info_()} is the standard-evaluation version of
#' \code{arg_info()} and takes a symbol and a call stack object.
#'
#' @param x An argument of a function.
#' @return A list containing:
#' \itemize{
#'   \item \code{expr}, the expression provided in the original
#'     call. If the argument was missing, \code{expr} is the default
#'     argument of the function; if there was no default, \code{expr}
#'     is \code{NULL}.
#'
#'   \item \code{env}, the environment in which \code{expr} should be
#'     evaluated. This is either the environment of the original
#'     calling frame, or the execution frame of the first function
#'     that was called in case the argument was missing. The
#'     difference reflects the evaluation rules of R, where default
#'     arguments are scoped within the called function.
#'
#'   \item \code{caller_frame}, the original calling frame. The
#'     environment of this frame is the same as \code{env} unless the
#'     argument was not supplied in the original call. In this case,
#'     \code{caller_frame$env} is the environment in which the
#'     argument would have been evaluated, had it been supplied.
#'
#'   \item \code{missing}, a boolean indicating whether the argument
#'     was not supplied there was no default argument. This allows you
#'     to differentiate an actual \code{NULL} argument from a missing
#'     one, without having to compare \code{env} and \code{caller_env}.
#' }
#' @seealso \code{\link{arg_label}()}, \code{\link{arg_expr}()},
#'   \code{\link{arg_env}()}.
#' @export
arg_info <- function(x) {
  stack <- call_stack()
  expr <- quote(x)
  arg_info_(expr, stack)
}

#' @rdname arg_info
#' @param expr A quoted symbol giving the name of the argument to
#'   introspect.
#' @param stack A \code{call_stack} object as returned by
#'   \code{\link{call_stack}()}.
#' @export
arg_info_ <- function(expr, stack) {
  stopifnot(is_call_stack(stack))

  calls <- lapply(stack, call_standardise, enum_dots = TRUE)
  missing <- FALSE

  for (i in seq_along(calls)) {
    frame <- stack[[i]]
    call <- calls[[i]]
    env <- frame$env

    if (!is.symbol(expr)) {
      break
    }

    arg <- arg_match(call, expr, frame$fn)
    if (is.null(arg)) {
      # Check for default arguments
      arg <- arg_default(expr, frame$fn)

      # If no default argument, mark argument as missing
      if (identical(arg, quote(`__missing`))) {
        missing <- TRUE
        expr <- NULL
      } else if (!is.null(arg)) {
        expr <- arg
      } else {
        break
      }

      # Since this is a missing argument, the environment in which the
      # expression is evaluated (the closure's execution environment)
      # and the caller environment are different
      if (length(stack) < i + 1){
        # TODO: Should we have a global frame object?
        frame <- NULL
      } else {
        frame <- stack[[i + 1]]
      }
      break
    }

    expr <- arg
  }

  list(
    expr = expr,
    env = env,
    missing = missing,
    caller_frame = frame
  )
}

arg_match <- function(call, sym, fun) {
  args <- cdr(call)
  nms <- names2(call[-1])

  arg_i <- match(as.character(sym), nms)
  args[[arg_i]]
}

arg_default <- function(expr, fn) {
  if (!is.symbol(expr)) {
    return(NULL)
  }

  sym <- as.character(expr)
  args <- formals(fn)
  default <- args[[sym]]

  if (missing(default)) {
    quote(`__missing`)
  } else {
    default
  }
}

#' @export
#' @rdname arg_label
arg_expr <- function(x) {
  arg_info(x)$expr
}

#' @rdname arg_label
#' @param default_env This argument is now deprecated and has no
#'   longer any effect since \code{arg_env()} will always return an
#'   environment.
#' @export
arg_env <- function(x, default_env) {
  arg_info(x)$env
}

#' Find the expression associated with an argument
#'
#' \code{arg_expr()} finds the full expression; \code{arg_text()}
#' turns the expression into a single string; \code{arg_label()}
#' formats it nicely for use in messages. \code{arg_env()} finds the
#' environment associated with the expression.
#'
#' These functions never force promises, and will work even if a
#' promise has previously been forced.
#'
#' @inheritParams arg_info
#' @export
#' @examples
#' # Unlike substitute(), arg_expr() finds the original expression
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) list(substitute(z), arg_expr(z))
#'
#' f(1 + 2 + 3)
#'
#' arg_label(10)
#' # Names a quoted with ``
#' arg_label(x)
#' # Strings are encoded
#' arg_label("a\nb")
#' # Expressions are captured
#' arg_label(a + b + c)
#' # Long expressions are collapsed
#' arg_label(foo({
#'   1 + 2
#'   print(x)
#' }))
arg_label <- function(x) {
  arg_label_(arg_expr(x))
}

arg_label_ <- function(x) {
  if (is.character(x)) {
    encodeString(x, quote = '"')
  } else if (is.atomic(x)) {
    format(x)
  } else if (is.name(x)) {
    paste0("`", as.character(x), "`")
  } else {
    chr <- deparse(x)
    if (length(chr) > 1) {
      dot_call <- call_new(x[[1]], quote(...))
      chr <- paste(deparse(dot_call), collapse = "\n")
    }
    paste0("`", chr, "`")
  }
}

#' @export
#' @rdname arg_label
#' @param width Width of each line.
#' @param nlines Maximum number of lines to extract.
arg_text <- function(x, width = 60L, nlines = Inf) {
  arg_text_(arg_expr(x), width = width, nlines = nlines)
}

arg_text_ <- function(x, width = 60L, nlines = Inf) {
  str <- deparse(x, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}

#' Generate a missing argument.
#'
#' @export
#' @examples
#' f_interp(~f(x = uq(arg_missing())))
#' f_interp(~f(x = uq(NULL)))
arg_missing <- function() {
  quote(expr = )
}
