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

    if (!is.symbol(expr)) {
      break
    }

    arg <- match_arg(call, expr, frame$fn)
    if (is.null(arg)) {
      # Check for default arguments
      expr <- arg_default(expr, frame$fun)

      # If no default argument, mark argument as missing
      if (is.null(expr)) {
        missing <- TRUE
      }

      # Since this is a missing argument, the environment in which the
      # expression is evaluated (the closure's execution environment)
      # and the caller environment are different
      calling_frame <- stack[[i + 1]]
      break
    }

    expr <- arg
    env <- sys.frame(frame$caller_pos)
    calling_frame <- frame
  }

  list(
    expr = expr,
    env = env,
    missing = missing,
    calling_frame = calling_frame
  )
}

# match.call() replaces dots by enumerated dots (e.g. ..1, ..2, etc).
# Add such names to arguments so that we can match against those
dots_names <- function(nms, fun) {
  is_dot <- which(!nms %in% names(formals(fun)))

  if (length(is_dot)) {
    nms[is_dot] <- paste0("..", seq_along(is_dot))
  }

  nms
}
is_dot <- function(sym) {
  grepl("^\\.\\.[0-9]+$", as.character(sym))
}
match_arg <- function(call, sym, fun) {
  nms <- names2(call[-1])
  if (is_dot(sym)) {
    nms <- dots_names(nms, fun)
  }

  arg_i <- match(as.character(sym), nms)
  if (is.na(arg_i)) {
    NULL
  } else {
    call[[arg_i + 1]]
  }
}

arg_default <- function(expr, fn) {
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
