#' Check argument type (language objects)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' These functions check that an argument is an R language object
#' (symbol, call, formula, environment, function, or closure) and
#' produce friendly error messages on failure.
#'
#' @param x The argument to check.
#' @param ... Additional arguments passed to [abort()].
#' @param allow_null Whether `NULL` is allowed.
#' @inheritParams args_error_context
#'
#' @return `NULL` invisibly if the check passes, throws an error otherwise.
#'
#' @seealso [check_type_scalar], [check_data_frame]
#'
#' @examples
#' # Succeeds
#' check_symbol(quote(foo))
#'
#' # Fails
#' try(check_symbol("foo"))
#'
#' @name check_type_lang
NULL

#' @rdname check_type_lang
#' @export
check_symbol <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a symbol",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_lang
#' @export
check_call <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_call(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused call",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_lang
#' @export
check_environment <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_environment(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an environment",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_lang
#' @export
check_function <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_function(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a function",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_lang
#' @export
check_closure <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_closure(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an R function",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' @rdname check_type_lang
#' @export
check_formula <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    if (is_formula(x, scoped = TRUE)) {
      return(invisible(NULL))
    }
    if (is_formula(x)) {
      cli::cli_abort(
        "{.arg {arg}} must be an evaluated formula, not a defused one.",
        arg = arg,
        call = call
      )
    }
  }

  stop_input_type(
    x,
    "a formula",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
