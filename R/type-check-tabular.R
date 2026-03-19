#' Check argument type (data frame)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Checks that an argument is a data frame, producing a friendly error
#' message on failure.
#'
#' @param x The argument to check.
#' @param ... Additional arguments passed to [abort()].
#' @param allow_null Whether `NULL` is allowed.
#' @inheritParams args_error_context
#'
#' @return `NULL` invisibly if the check passes, throws an error otherwise.
#'
#' @family input checkers
#'
#' @examples
#' check_data_frame(mtcars)
#'
#' try(check_data_frame(1:5))
#' @export
check_data_frame <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is.data.frame(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a data frame",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
