#' Find the expression associated with an argument
#'
#' \code{expr_find()} finds the full expression; \code{expr_text()} turns the
#' expression into a single string; \code{expr_label()} formats it nicely for
#' use in messages. \code{expr_env()} finds the environment associated with
#' the expression.
#'
#' These functions never force promises, and will work even if a promise has
#' previously been forced.
#'
#' @param x A promise (function argument)
#' @export
#' @examples
#' # Unlike substitute(), expr_find() finds the original expression
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) list(substitute(z), expr_find(z))
#'
#' f(1 + 2 + 3)
#'
#' expr_label(10)
#' # Names a quoted with ``
#' expr_label(x)
#' # Strings are encoded
#' expr_label("a\nb")
#' # Expressions are captured
#' expr_label(a + b + c)
#' # Long expressions are collapsed
#' expr_label(foo({
#'   1 + 2
#'   print(x)
#' }))
expr_label <- function(x) {
  expr_label_(expr_find(x))
}

expr_label_ <- function(x) {
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
#' @rdname expr_label
#' @param width Width of each line
#' @param nlines Maximum number of lines to extract.
expr_text <- function(x, width = 60L, nlines = Inf) {
  expr_text_(expr_find(x), width = width, nlines = nlines)
}

expr_text_ <- function(x, width = 60L, nlines = Inf) {
  str <- deparse(x, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}

match_arg <- function(call, sym) {
  args <- call[-1]
  arg_i <- match(as.character(sym), names(args))
  if (is.na(arg_i)) {
    NULL
  } else {
    args[[arg_i]]
  }
}

arg_info <- function(expr) {
  stack <- call_stack()
  calls <- lapply(stack, call_standardise, enum_dots = TRUE)
  expr <- quote(expr)

  frame <- stack[[11]]
  call_standardise(stack[[11]], enum_dots = TRUE)

  for (i in seq_along(calls)) {
    frame <- stack[[i]]
    call <- calls[[i]]

    if (!is.symbol(expr)) {
      break
    }

    arg <- match_arg(call, expr)
    if (is.null(arg)) {
      # Check for default arguments (which are evaluated within the
      # closure's execution environment)
      default <- default_arg(expr, frame$fn)
      if (!is.null(default)) {
        expr <- default
        env <- frame$env
      }
      break
    }

    expr <- arg
    env <- sys.frame(frame$caller_pos)
  }

  list(env = env, expr = expr)
}

default_arg <- function(expr, fn) {
  if (is.symbol(expr)) {
    sym <- as.character(expr)
    args <- formals(fn)
    default <- args[[sym]]

    if (!missing(default)) {
      return(default)
    }
  }

  NULL
}

#' @export
#' @rdname expr_label
expr_find <- function(x) {
  arg_info(x)$expr
}

#' @rdname expr_label
#' @param default_env This argument is now deprecated and has no
#'   longer any effect since \code{expr_env()} will always return an
#'   environment.
#' @export
expr_env <- function(x, default_env) {
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
