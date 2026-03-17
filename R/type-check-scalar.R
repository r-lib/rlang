#' Check argument type (scalar)
#'
#' These functions check that an argument is of the expected scalar type
#' and produce friendly error messages otherwise.
#'
#' @param x The argument to check.
#' @param ... Additional arguments passed to [abort()].
#' @inheritParams args_error_context
#' @param allow_na Whether `NA` values are acceptable.
#' @param allow_null Whether `NULL` is acceptable.
#' @param allow_empty Whether the empty string `""` is acceptable
#'   (`check_string()` only).
#' @param min Minimum value (inclusive). If `NULL`, no lower bound is
#'   checked.
#' @param max Maximum value (inclusive). If `NULL`, no upper bound is
#'   checked.
#' @param allow_infinite Whether infinite values are acceptable.
#'
#' @return `NULL` invisibly if the check passes, throws an error otherwise.
#'
#' @seealso [check_type_lang], [check_data_frame]
#'
#' @examples
#' check_bool(TRUE)
#' try(check_bool(1))
#'
#' check_string("hello")
#' try(check_string(42))
#'
#' check_number_decimal(3.14)
#' try(check_number_decimal("x"))
#'
#' check_number_whole(42)
#' try(check_number_whole(3.5))
#'
#' @name check_type_scalar
NULL

#' @rdname check_type_scalar
#' @export
check_bool <- function(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (
    !missing(x) &&
      .Call(
        ffi_standalone_is_bool_1.0.7,
        x,
        allow_na,
        allow_null
      )
  ) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_scalar
#' @export
check_string <- function(
  x,
  ...,
  allow_empty = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = allow_empty,
      allow_na = allow_na,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a single string",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_scalar
#' @export
check_name <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a valid name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_scalar
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

#' @rdname check_type_scalar
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


# Internal helpers --------------------------------------------------------

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

.rlang_check_is_string <- function(x, allow_empty, allow_na, allow_null) {
  if (is_string(x)) {
    if (allow_empty || !is_string(x, "")) {
      return(TRUE)
    }
  }

  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  if (allow_na && (identical(x, NA) || identical(x, na_chr))) {
    return(TRUE)
  }

  FALSE
}

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