#' Create a symbol or list of symbols
#'
#' These functions take strings as input and turn them into symbols.
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
#' @param x A string or symbol. If a string, the attributes are
#'   removed, if any.
#' @return A character vector of length 1.
#'
#' @section Unicode tags:
#'
#' Unlike [base::as.symbol()] and [base::as.name()], `as_string()`
#' automatically transforms unicode tags such as `"<U+5E78>"` to the
#' proper UTF-8 character. This is important on Windows because:
#'
#' * R on Windows has no UTF-8 support, and uses native encoding instead.
#'
#' * The native encodings do not cover all Unicode characters. For
#'   example, Western encodings do not support CKJ characters.
#'
#' * When a lossy UTF-8 -> native transformation occurs, uncovered
#'   characters are transformed to an ASCII unicode tag like `"<U+5E78>"`.
#'
#' * Symbols are always encoded in native. This means that
#'   transforming the column names of a data frame to symbols might be
#'   a lossy operation.
#'
#' * This operation is very common in the tidyverse because of data
#'   masking APIs like dplyr where data frames are transformed to
#'   environments. While the names of a data frame are stored as a
#'   character vector, the bindings of environments are stored as
#'   symbols.
#'
#' Because it reencodes the ASCII unicode tags to their UTF-8
#' representation, the string -> symbol -> string roundtrip is
#' more stable with `as_string()`.
#'
#' @seealso [as_name()] for a higher-level variant of `as_string()`
#'   that automatically unwraps quosures.
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
#' @export
as_string <- function(x) {
  if (is_string(x)) {
    attributes(x) <- NULL
    return(x)
  }

  if (is_symbol(x)) {
    return(.Call(rlang_sym_as_character, x))
  }

  abort_coercion(x, "a string")
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
