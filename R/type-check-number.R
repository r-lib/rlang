#' Check argument type (numbers)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' These functions check that an argument is a number, optionally
#' with bounds, and produce friendly error messages otherwise.
#'
#' @param x The argument to check.
#' @param ... Additional arguments passed to [abort()].
#' @param min Minimum value (inclusive). If `NULL`, no lower bound is
#'   checked.
#' @param max Maximum value (inclusive). If `NULL`, no upper bound is
#'   checked.
#' @param allow_infinite Whether infinite values are allowed.
#' @param allow_na Whether `NA` values are allowed.
#' @param allow_null Whether `NULL` is allowed.
#' @inheritParams args_error_context
#'
#' @return `NULL` invisibly if the check passes, throws an error otherwise.
#'
#' @family input checkers
#'
#' @examples
#' check_number_decimal(3.14)
#' try(check_number_decimal("x"))
#'
#' check_number_whole(42)
#' try(check_number_whole(3.5))
#'
#' @name check_type_number
NULL

#' @rdname check_type_number
#' @export
check_number_decimal <- function(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (
    0 ==
      (exit_code <- .Call(
        ffi_standalone_check_number_1.0.7,
        x,
        allow_decimal = TRUE,
        min,
        max,
        allow_infinite,
        allow_na,
        allow_null
      ))
  ) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = TRUE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_number
#' @export
check_number_whole <- function(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = FALSE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (
    0 ==
      (exit_code <- .Call(
        ffi_standalone_check_number_1.0.7,
        x,
        allow_decimal = FALSE,
        min,
        max,
        allow_infinite,
        allow_na,
        allow_null
      ))
  ) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = FALSE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

.stop_not_number <- function(
  x,
  ...,
  exit_code,
  allow_decimal,
  min,
  max,
  allow_na,
  allow_null,
  arg,
  call
) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  if (exit_code == IS_NUMBER_oob) {
    min <- min %||% -Inf
    max <- max %||% Inf

    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min, max)
    } else if (x < min) {
      what <- sprintf("%s larger than or equal to %s", what, min)
    } else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s", what, max)
    } else {
      abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }

  stop_input_type(
    x,
    what,
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
