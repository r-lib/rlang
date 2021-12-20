#' Create a symbol or list of symbols
#'
#' @description
#'
#' Symbols are a kind of [defused expression][topic-defuse] that
#' represent objects in environments.
#'
#' * `sym()` and `syms()` take strings as input and turn them into
#'   symbols.
#'
#' * `data_sym()` and `data_syms()` create calls of the form
#'   `.data$foo` instead of symbols. Subsetting the [`.data`] pronoun
#'   is more robust when you expect a data-variable. See
#'   `r link("topic_data_mask_ambiguity")`.
#'
#' Only tidy eval APIs support the [`.data`] pronoun. With base R
#' functions, use simple symbols created with `sym()` or `syms()`.
#'
#' @param x For `sym()` and `data_sym()`, a string. For `syms()` and
#'   `data_syms()`, a list of strings.
#' @return For `sym()` and `syms()`, a symbol or list of symbols. For
#'   `data_sym()` and `data_syms()`, calls of the form `.data$foo`.
#'
#' @seealso
#' - `r link("topic_defuse")`
#' - `r link("topic_metaprogramming")`
#'
#' @examples
#' # Create a symbol
#' sym("cyl")
#'
#' # Create a list of symbols
#' syms(c("cyl", "am"))
#'
#' # Symbolised names refer to variables
#' eval(sym("cyl"), mtcars)
#'
#' # Beware of scoping issues
#' Cyl <- "wrong"
#' eval(sym("Cyl"), mtcars)
#'
#' # Data symbols are explicitly scoped in the data mask
#' try(eval_tidy(data_sym("Cyl"), mtcars))
#'
#' # These can only be used with tidy eval functions
#' try(eval(data_sym("Cyl"), mtcars))
#'
#' # The empty string returns the missing argument:
#' sym("")
#'
#' # This way sym() and as_string() are inverse of each other:
#' as_string(missing_arg())
#' sym(as_string(missing_arg()))
#'
#' @export
sym <- function(x) {
  if (is_symbol(x)) {
    return(x)
  }
  if (identical(x, "")) {
    return(missing_arg())
  }
  if (!is_string(x)) {
    abort_coercion(x, "a symbol")
  }
  .Call(ffi_symbol, x)
}
#' @rdname sym
#' @export
syms <- function(x) {
  map(x, sym)
}

#' @rdname sym
#' @export
data_sym <- function(x) {
  call("$", quote(.data), sym(x))
}
#' @rdname sym
#' @export
data_syms <- function(x) {
  map(x, data_sym)
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
    return(.Call(ffi_sym_as_character, x))
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
