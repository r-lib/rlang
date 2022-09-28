# nocov start --- r-lib/rlang compat-types-check
#
# Dependencies
# ============
#
# - compat-obj-type.R
#
# Changelog
# =========
#
# 2022-09-28:
# - Removed `what` arguments.
# - Added `allow_na` and `allow_null` arguments.
#
#
# 2022-09-16:
# - Unprefixed usage of rlang functions with `rlang::` to
#   avoid onLoad issues when called from rlang (#1482).
#
# 2022-08-11:
# - Added changelog.

# Scalars -----------------------------------------------------------------

check_bool <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (is_bool(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }
  if (allow_na && identical(x, NA)) {
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

check_string <- function(x,
                         ...,
                         allow_empty = TRUE,
                         allow_na = FALSE,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (is_string(x)) {
    if (allow_empty || !is_string(x, "")) {
      return(invisible(NULL))
    }
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }
  if (allow_na && (identical(x, NA) ||
                   identical(x, na_chr))) {
    return(invisible(NULL))
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

check_number <- function(x,
                         ...,
                         allow_na = FALSE,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (is_number(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }
  if (allow_na && (identical(x, NA) ||
                   identical(x, na_dbl) ||
                   identical(x, na_int))) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a round number",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

is_number <- function(x) {
  is_integerish(x, n = 1, finite = TRUE)
}

check_symbol <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (is_symbol(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a symbol",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_arg <- function(x,
                      ...,
                      allow_null = FALSE,
                      arg = caller_arg(x),
                      call = caller_env()) {
  if (is_symbol(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "an argument name",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_call <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (is_call(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a defused call",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_environment <- function(x,
                              ...,
                              allow_null = FALSE,
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (is_environment(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "an environment",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_function <- function(x,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (is_function(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a function",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_closure <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (is_closure(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "an R function",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_formula <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (is_formula(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a formula",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


# Vectors -----------------------------------------------------------------

check_character <- function(x,
                            ...,
                            allow_null = FALSE,
                            arg = caller_arg(x),
                            call = caller_env()) {
  if (is_character(x)) {
    return(invisible(NULL))
  }
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a character vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# nocov end
