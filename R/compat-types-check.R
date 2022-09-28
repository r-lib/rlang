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
#
# - Removed `what` arguments.
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
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!is_bool(x)) {
    stop_input_type(x, "`TRUE` or `FALSE`", ..., arg = arg, call = call)
  }
}

check_string <- function(x,
                         ...,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!is_string(x)) {
    stop_input_type(x, "a single string", ..., arg = arg, call = call)
  }
}

check_number <- function(x,
                         ...,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!is_number(x)) {
    stop_input_type(x, "a round number", ..., arg = arg, call = call)
  }
}

is_number <- function(x) {
  is_integerish(x, n = 1, finite = TRUE)
}

check_symbol <- function(x,
                         ...,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!is_symbol(x)) {
    stop_input_type(x, "a symbol", ..., arg = arg, call = call)
  }
}

check_arg <- function(x,
                      ...,
                      arg = caller_arg(x),
                      call = caller_env()) {
  if (!is_symbol(x)) {
    stop_input_type(x, "an argument name", ..., arg = arg, call = call)
  }
}

check_call <- function(x,
                       ...,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!is_call(x)) {
    stop_input_type(x, "a defused call", ..., arg = arg, call = call)
  }
}

check_environment <- function(x,
                              ...,
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (!is_environment(x)) {
    stop_input_type(x, "an environment", ..., arg = arg, call = call)
  }
}

check_function <- function(x,
                           ...,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!is_function(x)) {
    stop_input_type(x, "a function", ..., arg = arg, call = call)
  }
}

check_closure <- function(x,
                          ...,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!is_closure(x)) {
    stop_input_type(x, "an R function", ..., arg = arg, call = call)
  }
}

check_formula <- function(x,
                          ...,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!is_formula(x)) {
    stop_input_type(x, "a formula", ..., arg = arg, call = call)
  }
}


# Vectors -----------------------------------------------------------------

check_character <- function(x,
                            ...,
                            arg = caller_arg(x),
                            call = caller_env()) {
  if (!is_character(x)) {
    stop_input_type(x, "a character vector", ..., arg = arg, call = call)
  }
}

# nocov end
