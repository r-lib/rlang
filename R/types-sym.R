#' Create a symbol or list of symbols.
#'
#' These functions take strings as input and turn them into symbols.
#'
#' @param x A string or list of strings.
#' @return A symbol for \code{symbol()} and a list of symbols for
#'   \code{symbols()}.
#' @useDynLib rlang rlang_symbol
#' @export
symbol <- function(x) {
  if (is_symbol(x)) {
    return(x)
  }
  if (!is_string(x)) {
    abort("Only strings can be converted to symbols")
  }
  .Call(rlang_symbol, x)
}
#' @rdname symbol
#' @export
symbols <- function(x) {
  map(x, symbol)
}
