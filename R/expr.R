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

#' @useDynLib lazyeval expr_find_
#' @export
#' @rdname expr_label
expr_find <- function(x) {
  .Call(expr_find_, quote(x), environment())
}

#' @useDynLib lazyeval expr_env_
#' @param default_env If supplied, \code{expr_env} will return this if the
#'   promise has already been forced. Otherwise it will throw an error.
#' @export
#' @rdname expr_label
expr_env <- function(x, default_env) {
  env <- .Call(expr_env_, quote(x), environment())

  if (is.null(env)) {
    if (missing(default_env)) {
      stop("Promise has already been forced")
    } else {
      default_env
    }
  } else {
    env
  }
}
