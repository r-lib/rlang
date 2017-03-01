#' Create a symbol or list of symbols.
#'
#' These functions take strings as input and turn them into symbols.
#'
#' @param x A string or list of strings.
#' @return A symbol for \code{symbol()} and a list of symbols for
#'   \code{symbols()}.
#' @export
symbol <- function(x) {
  if (is_symbol(x)) {
    return(x)
  }
  if (!is_string(x)) {
    abort("Only strings can be converted to symbols")
  }
  as.name(x)
}
#' @rdname symbol
#' @export
symbols <- function(x) {
  map(x, symbol)
}

sym_namespace <- quote(`::`)
sym_namespace2 <- quote(`:::`)
sym_dollar <- quote(`$`)
sym_at <- quote(`@`)
sym_tilde <- quote(`~`)
