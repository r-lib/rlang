#' Create a symbol or list of symbols.
#'
#' These functions take strings as input and turn them into symbols.
#' Contrarily to `as.name()`, they convert the strings to the native
#' encoding beforehand. This is necessary because symbols remove
#' silently the encoding mark of strings (see [set_str_encoding()]).
#'
#' @param x A string or list of strings.
#' @return A symbol for `sym()` and a list of symbols for `syms()`.
#' @useDynLib rlang rlang_symbol
#' @export
sym <- function(x) {
  if (is_symbol(x)) {
    return(x)
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

#' Coerce a symbol to a name
#'
#' @param x A symbol.
#' @return A string.
#' @seealso [sym()] for the inverse operation.
#' @export
#' @examples
#' sym <- quote(sym)
#' as_name(sym)
#'
#' quo <- quo(sym)
#' as_name(get_expr(quo))
as_name <- function(x) {
  coerce_type(x, "a name",
    string = x,
    symbol = as_string(x)
  )
}

#' @rdname is_expr
#' @export
is_symbol <- function(x) {
  typeof(x) == "symbol"
}

sym_namespace <- quote(`::`)
sym_namespace2 <- quote(`:::`)
sym_dollar <- quote(`$`)
sym_at <- quote(`@`)
sym_tilde <- quote(`~`)
sym_def <- quote(`:=`)
sym_curly <- quote(`{`)
