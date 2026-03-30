#' Check argument type (scalar)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' These functions check that an argument is of the expected scalar type
#' and produce friendly error messages otherwise.
#'
#' @param x The argument to check.
#' @param ... Additional arguments passed to [abort()].
#' @inheritParams args_error_context
#' @param allow_na Whether `NA` values are allowed.
#' @param allow_null Whether `NULL` is allowed.
#' @param allow_empty Whether the empty string `""` is allowed
#'   (`check_string()` only).
#'
#' @return `NULL` invisibly if the check passes, throws an error otherwise.
#'
#' @family input checkers
#'
#' @examples
#' check_bool(TRUE)
#' try(check_bool(1))
#'
#' check_string("hello")
#' try(check_string(42))
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
