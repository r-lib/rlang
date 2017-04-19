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
#'
#' as_lang(~ f)
#' as_lang(quo(f))
#'
#' # as_symbol() takes the function name of quoted calls:
#' as_symbol(~ f())
#' as_symbol(quo(f()))
as_symbol <- function(x) {
  coerce_type(x, "symbol",
    symbol = x,
    string = sym(x),
    formula = as_symbol(f_rhs(x)),
    language =
      switch_lang(x,
        namespaced = node_car(x),
        inlined = abort("Can't create symbol from inlined call"),
        recursive = abort("Can't create symbol from recursive call"),
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
  typeof(x) == "symbol"
}

sym_namespace <- quote(`::`)
sym_namespace2 <- quote(`:::`)
sym_dollar <- quote(`$`)
sym_at <- quote(`@`)
sym_tilde <- quote(`~`)
sym_def <- quote(`:=`)
sym_curly <- quote(`{`)
