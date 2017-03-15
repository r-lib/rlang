#' Create a symbol or list of symbols.
#'
#' These functions take strings as input and turn them into symbols.
#' Contrarily to `as.name()`, they convert the strings to the native
#' encoding beforehand. This is necessary because symbols remove
#' silently the encoding mark of strings (see [str_set_encoding()]).
#'
#' @param x A string or list of strings.
#' @return A symbol for `symbol()` and a list of symbols for
#'   `symbols()`.
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

#' Coerce an object to a name or call.
#'
#' These coercing functions can transform names, calls, formulas, and
#' strings. The distinction between a name and a call is particularly
#' important when coercing from a string. Coercing to a call will
#' parse the string, coercing to a name will create a (potentially)
#' non-syntactic name.
#'
#' @param x An object to coerce
#' @export
#' @return `as_symbol()` and `as_lang()` return a symbol or a
#'   call. `as_name()` returns a string.
#' @examples
#' as_symbol("x + y")
#' as_lang("x + y")
#'
#' as_lang(~ f)
#' as_symbol(~ f())
as_symbol <- function(x) {
  coerce_type(x, "symbol",
    symbol = x,
    string = symbol(x),
    quosure = as_symbol(f_rhs(x)),
    language =
      switch_lang(x,
        namespaced = node_car(x),
        inlined = abort("Cannot create symbol from inlined call"),
        recursive = abort("cannot create symbol from recursive call"),
        as_symbol(node_car(x))
      )
  )
}
#' @export
#' @rdname as_symbol
as_name <- function(x) {
  switch_type(x,
    string = x,
    as_string(as_symbol(x))
  )
}

#' @rdname is_expr
#' @export
is_symbol <- function(x) {
  x <- get_expr(x)
  typeof(x) == "symbol"
}

sym_namespace <- quote(`::`)
sym_namespace2 <- quote(`:::`)
sym_dollar <- quote(`$`)
sym_at <- quote(`@`)
sym_tilde <- quote(`~`)
sym_def <- quote(`:=`)
