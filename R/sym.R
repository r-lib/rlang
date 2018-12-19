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
#' @param name An optional name or vector of names that the symbol
#'   should match.
#' @export
is_symbol <- function(x, name = NULL) {
  if (typeof(x) != "symbol") {
    return(FALSE)
  }
  if (is_null(name)) {
    return(TRUE)
  }
  as_string(x) %in% name
}

#' Cast symbol to string
#'
#' `as_string()` converts [symbols][sym] to character strings.
#'
#' @param x A string or symbol, possibly wrapped in a [quosure][quosure].
#'   If a string, the attributes are removed, if any.
#' @return A character vector of length 1.
#'
#' @examples
#' # Let's create some symbols:
#' foo <- quote(foo)
#' bar <- sym("bar")
#'
#' # as_string() converts symbols to strings:
#' foo
#' as_string(foo)
#'
#' typeof(bar)
#' typeof(as_string(bar))
#'
#' # as_string() unwraps quosured symbols automatically:
#' as_string(quo(foo))
#' @export
as_string <- function(x) {
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }
  coerce_type(x, "a string",
    symbol = {
      .Call(rlang_symbol_to_character, x)
    },
    string = {
      attributes(x) <- NULL
      x
    }
  )
}

namespace_sym <- quote(`::`)
namespace2_sym <- quote(`:::`)
dollar_sym <- quote(`$`)
dot_data_sym <- quote(.data)
dots_sym <- quote(...)
at_sym <- quote(`@`)
tilde_sym <- quote(`~`)
colon_equals_sym <- quote(`:=`)
brace_sym <- quote(`{`)
dots_sym <- quote(...)
function_sym <- quote(`function`)
dot_sym <- quote(.)
pipe_sym <- quote(`%>%`)
