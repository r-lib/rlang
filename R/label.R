#' Find the expression associated with an argument
#'
#' \code{expr_find()} finds the full expression; \code{expr_label()} formats it
#' nicely for use in output.
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
  x <- expr_find(x)

  if (is.character(x)) {
    encodeString(x, quote = '"')
  } else if (is.atomic(x)) {
    format(x)
  } else if (is.name(x)) {
    paste0("`", as.character(x), "`")
  } else {
    chr <- deparse(x)
    if (length(chr) > 1) {
      chr <- paste(deparse(as.call(list(x[[1]], quote(...)))), collapse = "\n")
    }
    chr
  }
}

#' @useDynLib lazyeval find_expr_
#' @export
#' @rdname expr_label
expr_find <- function(x) {
  .Call(find_expr_, quote(x), environment())
}
