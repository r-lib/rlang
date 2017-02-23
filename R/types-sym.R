#' Create a symbol or list of symbols.
#'
#' These functions take strings as input and turn them into symbols.
#' Contrarily to \code{as.name()}, they convert the strings to the
#' native encoding beforehand. This is necessary because symbols
#' remove silently the encoding mark of strings (see
#' \code{\link{str_set_encoding}()}).
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

sym_namespace <- quote(`::`)
sym_namespace2 <- quote(`:::`)
sym_dollar <- quote(`$`)
sym_at <- quote(`@`)
sym_tilde <- quote(`~`)
