#' Inspect an argument
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
#' @param x An argument to a function.
#' @return A list containing:
#'   \item{expr}{The expression provided in the original call. If the
#'     argument was missing, \code{expr} is the default argument of
#'     the function; if there was no default, \code{expr} is
#'     \code{NULL}.}
#'
#'   \item{eval_frame}{The frame providing the scope for \code{expr},
#'     which should normally be evaluated in \code{eval_frame$env}.
#'     This is either the original calling frame, or the execution
#'     frame of the function that was called if the argument was
#'     missing. The difference reflects the evaluation rules of R,
#'     where default arguments are scoped within the called function
#'     rather than the calling frame.}
#'
#'   \item{caller_frame}{The original calling frame. This is the same
#'     as \code{eval_frame} unless no argument was supplied in the
#'     original call. In this case, \code{caller_frame$env} is the
#'     environment in which the argument would have been evaluated,
#'     had it been supplied.}
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
#'   inspect.
#' @param stack A \code{call_stack} object as returned by
#'   \code{\link{call_stack}()}.
#' @export
arg_info_ <- function(expr, stack) {
  stopifnot(is_call_stack(stack))
  stopifnot(length(stack) > 1)

  calls <- lapply(drop_last(stack), call_standardise, enum_dots = TRUE)
  caller_frame <- stack[[1]]
  eval_frame <- stack[[1]]
  prev_expr <- expr

  # In this loop `prev_expr` is the argument of the frame just before
  # the current `i`th frame, the tentative caller frame
  for (i in seq_along(calls)) {
    call <- calls[[i]]

    # If `prev_expr` is a complex expression, we know that (a) we have
    # reached the caller frame, and (b) there is no default argument
    if (!is.symbol(maybe_missing(prev_expr))) {
      break
    }


    if (missing(prev_expr)) {
      arg_i <- NA
    } else {
      arg_i <- arg_match(prev_expr, call)
    }

    # If no match in the call, we have reached the caller frame
    if (is.na(arg_i)) {
      if (!missing(prev_expr)) {
        # Check for default argument
        fml_i <- fml_match(prev_expr, eval_frame$fn)
        if (!is.na(fml_i)) {
          prev_expr <- fml_default(prev_expr, eval_frame$fn) %||% prev_expr
          caller_frame <- stack[[i + 1]]
        }
      }

      break
    }

    caller_arg <- call[[arg_i]]

    # The matched argument is missing. Check for default arguments.
    # The caller frame is the next frame.
    if (missing(caller_arg)) {
      prev_expr <- fml_default(prev_expr, eval_frame$fn) %||% prev_expr
      caller_frame <- stack[[i + 1]]
      break
    }

    # If one argument in the caller signature matches, record
    # corresponding expression and move on to next frame
    prev_expr <- caller_arg
    caller_frame <- stack[[i + 1]]
    eval_frame <- stack[[i + 1]]
  }

  list(
    expr = maybe_missing(prev_expr),
    eval_frame = eval_frame,
    caller_frame = caller_frame
  )
}

arg_match <- function(sym, call) {
  arg_nm <- as.character(sym)
  match(arg_nm, names2(call))
}
fml_match <- function(sym, fn) {
  arg_nm <- as.character(sym)
  match(arg_nm, names(formals(fn)))
}
fml_default <- function(expr, fn) {
  nm <- as.character(expr)
  fmls <- formals(fn)
  fmls[[nm]]
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
  arg_info(x)$eval_frame$env
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

#' Generate or handle a missing argument
#'
#' These functions help using the missing argument as a regular R
#' object. It is valid to generate a missing argument and assign it in
#' the current environment or in a list. However, once assigned in the
#' environment, the missing argument normally cannot be
#' touched. \code{maybe_missing()} checks whether the object is the
#' missing argument, and regenerate it if needed to prevent R from
#' throwing a missing error. In addition, \code{is_missing()} lets you
#' check for a missing argument in a larger range of situations than
#' \code{\link[base]{missing}()} (see examples).
#' @param x An object that might be the missing argument.
#' @export
#' @examples
#' # The missing argument can be useful to generate calls
#' f_interp(~f(x = uq(arg_missing())))
#' f_interp(~f(x = uq(NULL)))
#'
#'
#' # It is perfectly valid to generate and assign the missing
#' # argument.
#' x <- arg_missing()
#' l <- list(arg_missing())
#'
#' # Note that accessing a missing argument contained in a list does
#' # not trigger an error:
#' l[[1]]
#' is.null(l[[1]])
#'
#' # But if the missing argument is assigned in the current
#' # environment, it is no longer possible to touch it. The following
#' # lines would all return errors:
#' #> x
#' #> is.null(x)
#'
#' # In these cases, you can use maybe_missing() to manipulate an
#' # object that might be the missing argument without triggering a
#' # missing error:
#' maybe_missing(x)
#' is.null(maybe_missing(x))
#' is_missing(maybe_missing(x))
#'
#'
#' # base::missing() does not work well if you supply an
#' # expression. The following lines would throw an error:
#'
#' #> missing(arg_missing())
#' #> missing(l[[1]])
#'
#' # while is_missing() will work as expected:
#' is_missing(arg_missing())
#' is_missing(l[[1]])
arg_missing <- function() {
  quote(expr = )
}

#' @rdname arg_missing
#' @export
is_missing <- function(x) {
  expr <- substitute(x)
  if (is.symbol(expr) && missing(x)) {
    TRUE
  } else {
    identical(x, arg_missing())
  }
}

#' @rdname arg_missing
#' @export
maybe_missing <- function(x) {
  if (is_missing(x)) {
    arg_missing()
  } else {
    x
  }
}
