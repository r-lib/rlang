#' Throw a type mismatch error
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `stop_input_type()` throws an error when an argument has the wrong
#' type, producing a friendly error message that includes the expected
#' type and the actual type of the input.
#'
#' @param x The object that does not conform to `what`. Its
#'   "friendly type" is used in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param ... Additional arguments passed to [abort()].
#' @param allow_na If `TRUE`, an `NA` description is appended to
#'   `what` in the error message.
#' @param allow_null If `TRUE`, a `NULL` description is appended to
#'   `what` in the error message.
#' @param show_value Whether to show the actual value in the error
#'   message.
#' @inheritParams args_error_context
#'
#' @return Throws an error, does not return.
#'
#' @family input checkers
#'
#' @examples
#' check_character <- function(
#'   x,
#'   ...,
#'   allow_null = FALSE,
#'   arg = caller_arg(x),
#'   call = caller_env()
#' ) {
#'   if (!missing(x)) {
#'     if (is_character(x)) {
#'       return(invisible(NULL))
#'     }
#'     if (allow_null && is_null(x)) {
#'       return(invisible(NULL))
#'     }
#'   }
#'
#'   stop_input_type(
#'     x,
#'     "a character vector",
#'     ...,
#'     allow_null = allow_null,
#'     arg = arg,
#'     call = call
#'   )
#' }
#'
#' # Succeeds
#' check_character(letters)
#'
#' # Fails
#' try(check_character(42))
#' @export
stop_input_type <- function(
  x,
  what,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  show_value = TRUE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_na) {
    what <- c(what, format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  if (inherits(arg, "AsIs")) {
    fmt_arg <- identity
  } else {
    fmt_arg <- format_arg
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    fmt_arg(arg),
    what,
    obj_type_friendly(x, value = show_value)
  )

  abort(message, ..., call = call, arg = arg)
}
