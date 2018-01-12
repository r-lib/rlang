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
#' @param name An optional name that the symbol should match.
#' @export
is_symbol <- function(x, name = NULL) {
  if (typeof(x) != "symbol") {
    return(FALSE)
  }
  if (!is_null(name) && !identical(as_string(x), name)) {
    return(FALSE)
  }
  TRUE
}

#' Capture a symbol
#'
#' @description
#'
#' * Like [enexpr()] and [enquo()], `ensym()` makes an argument
#'   auto-quoting. It always returns a symbol and issues an error if
#'   the input is not a string or a symbol. It supports
#'   [quasiquotation].
#'
#' * Like [enexprs()] and [enquos()], `ensyms()` captures multiple
#'   arguments at once. In particular it captures all arguments passed
#'   in `...`. If any of the captured argument is not a symbol, it
#'   throws an error.
#'
#' @inheritParams expr
#' @inheritParams exprs
#'
#' @export
#' @examples
#' quote_sym <- function(arg) ensym(arg)
#' quote_sym(foo)
#' quote_sym("foo")
#'
#' # The ensymed argument is quasiquoted:
#' var <- "foo"
#' quote_sym(var)
#' quote_sym(!!var)
ensym <- function(arg) {
  .Call(rlang_ensym, substitute(arg), parent.frame())
}
#' @rdname ensym
#' @export
ensyms <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE) {
  exprs <- endots(
    environment(),
    parent.frame(),
    rlang_enexpr,
    rlang_exprs_interp,
    .named,
    .ignore_empty,
    .unquote_names
  )
  if (!every(exprs, function(x) is_symbol(x) || is_string(x))) {
    abort("Must supply symbols or strings as argument")
  }
  map(exprs, sym)
}

namespace_sym <- quote(`::`)
namespace2_sym <- quote(`:::`)
dollar_sym <- quote(`$`)
at_sym <- quote(`@`)
tilde_sym <- quote(`~`)
colon_equals_sym <- quote(`:=`)
curly_sym <- quote(`{`)
dots_sym <- quote(...)
