#' Create a symbol or list of symbols
#'
#' These functions take strings as input and turn them into symbols.
#' Contrarily to `as.name()`, they convert the strings to the native
#' encoding beforehand. This is necessary because symbols remove
#' silently the encoding mark of strings (see [set_str_encoding()]).
#'
#' @param x A string or list of strings.
#' @return A symbol for `sym()` and a list of symbols for `syms()`.
#' @export
#' @examples
#' # The empty string returns the missing argument:
#' sym("")
#'
#' # This way sym() and as_string() are inverse of each other:
#' as_string(missing_arg())
#' sym(as_string(missing_arg()))
sym <- function(x) {
  if (is_symbol(x)) {
    return(x)
  }
  if (identical(x, "")) {
    return(missing_arg())
  }
  if (!is_string(x)) {
    abort("Only strings can be converted to symbols")
  }
  .Call(rlang_symbol, x)
}
#' @rdname sym
#' @export
syms <- function(x) {
  map(x, sym)
}

#' Is object a symbol?
#' @param x An object to test.
#' @export
is_symbol <- function(x) {
  typeof(x) == "symbol"
}

#' @rdname expr
#' @export
ensym <- function(arg) {
  .Call(rlang_ensym, substitute(arg), parent.frame())
}

sym_namespace <- quote(`::`)
sym_namespace2 <- quote(`:::`)
sym_dollar <- quote(`$`)
sym_at <- quote(`@`)
sym_tilde <- quote(`~`)
sym_def <- quote(`:=`)
sym_curly <- quote(`{`)
