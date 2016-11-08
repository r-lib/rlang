#' Find the expression associated with an argument
#'
#' \code{arg_find()} finds the full expression; \code{arg_text()} turns the
#' expression into a single string; \code{arg_label()} formats it nicely for
#' use in messages. \code{arg_env()} finds the environment associated with
#' the expression.
#'
#' These functions never force promises, and will work even if a promise has
#' previously been forced.
#'
#' @param x A promise (function argument)
#' @export
#' @examples
#' # Unlike substitute(), arg_find() finds the original expression
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) list(substitute(z), arg_find(z))
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
  arg_label_(arg_find(x))
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
#' @param width Width of each line
#' @param nlines Maximum number of lines to extract.
arg_text <- function(x, width = 60L, nlines = Inf) {
  arg_text_(arg_find(x), width = width, nlines = nlines)
}

arg_text_ <- function(x, width = 60L, nlines = Inf) {
  str <- deparse(x, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}

arg_info <- function(expr, n = NULL) {
  stack <- call_stack()
  if (!is.null(n)) {
    stopifnot(n > 0)
    stack <- stack[seq_len(n)]
  }
  expr <- quote(expr)
  arg_info_(expr, stack)
}

arg_info_ <- function(expr, stack) {
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
    calling_frame = frame
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
arg_find <- function(x) {
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

#' Generate a missing argument.
#'
#' @export
#' @examples
#' f_interp(~f(x = uq(arg_missing())))
#' f_interp(~f(x = uq(NULL)))
arg_missing <- function() {
  quote(expr = )
}
