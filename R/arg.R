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
#'   \item{caller_frame}{The original calling frame. This is the frame
#'     in which \code{expr} should be evaluated, unless the argument is
#'     missing (see below).}
#'
#'   \item{expr}{The expression provided in the original call. If the
#'     argument was missing, \code{expr} is the default argument of
#'     the function; if there was no default, \code{expr} is the
#'     missing argument (see \code{\link{arg_missing}()}).}
#'
#'   \item{name}{The name of the formal argument to which \code{expr}
#'     was originally supplied.}
#'
#'   \item{eval_frame}{The frame providing the scope for \code{expr},
#'     which should normally be evaluated in \code{eval_frame$env}.
#'     This is normally the original calling frame, unless the
#'     argument was missing. In that case, \code{eval_frame} is the
#'     evaluation frame of the called function. The difference
#'     reflects the evaluation rules of R, where default arguments are
#'     scoped within the called function rather than the calling
#'     frame.}
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

  calls <- lapply(drop_last(stack), call_standardise,
    enum_dots = TRUE, add_missings = TRUE)

  # In this loop `expr` is the argument of the frame just before
  # the current `i`th frame, the tentative caller frame
  caller_frame <- stack[[1]]
  eval_frame <- stack[[1]]
  for (i in seq_along(calls)) {
    call <- calls[[i]]

    # The `caller_expr` is always matched and valid during the first
    # iteration of the loop
    arg_i <- arg_match(expr, call)
    caller_expr <- call[[arg_i]]

    # If no match in the call, we have reached the caller frame.
    if (is.na(arg_i)) {
      break
    }

    # The matched argument is missing, either implicitely or
    # explicitely. The evaluation frame of missing arguments is the
    # current frame, but the caller is the next one
    if (missing(caller_expr)) {
      formal_name <- as.character(expr)
      expr <- fml_default(expr, eval_frame$fn)
      caller_frame <- stack[[i + 1]]
      break
    }

    # If `caller_expr` is a complex expression, we have reached the
    # callee frame, and the next frame is both the caller and
    # evaluation frame
    if (!is.symbol(caller_expr)) {
      formal_name <- as.character(expr)
      expr <- caller_expr
      caller_frame <- stack[[i + 1]]
      eval_frame <- stack[[i + 1]]
      break
    }

    # If the argument matched in the caller signature is another
    # symbol, record it and move on to next frame
    formal_name <- as.character(expr)
    expr <- caller_expr

    caller_frame <- stack[[i + 1]]
    eval_frame <- stack[[i + 1]]
  }

  list(
    expr = maybe_missing(expr),
    name = formal_name,
    eval_frame = eval_frame,
    caller_frame = caller_frame
  )
}

arg_match <- function(sym, call) {
  arg_nm <- as.character(sym)
  match(arg_nm, names2(call))
}
fml_default <- function(expr, fn) {
  nm <- as.character(expr)
  fmls <- formals(fn)
  if (nm %in% names(fmls)) {
    fmls[[nm]]
  } else {
    arg_missing()
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
