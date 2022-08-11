# Scalars -----------------------------------------------------------------

check_bool <- function(x,
                       ...,
                       what = "`TRUE` or `FALSE`",
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!is_bool(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_string <- function(x,
                         ...,
                         what = "a single string",
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!is_string(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_number <- function(x,
                         ...,
                         what = "a round number",
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!is_number(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_symbol <- function(x,
                         ...,
                         what = "a symbol",
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!is_symbol(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_arg <- function(x,
                      ...,
                      what = "an argument name",
                      arg = caller_arg(x),
                      call = caller_env()) {
  check_symbol(x, ..., what = what, arg = arg, call = call)
}

check_call <- function(x,
                       ...,
                       what = "a defused call",
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!is_call(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_environment <- function(x,
                              ...,
                              what = "an environment",
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (!is_environment(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_function <- function(x,
                           ...,
                           what = "a function",
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!is_function(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_closure <- function(x,
                           ...,
                           what = "an R function",
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!is_closure(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}

check_formula <- function(x,
                          ...,
                          what = "a formula",
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!is_formula(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}


# Vectors -----------------------------------------------------------------

# TODO: Restrict missing and special values

check_character <- function(x,
                            ...,
                            what = "a character vector",
                            arg = caller_arg(x),
                            call = caller_env()) {
  if (!is_character(x)) {
    stop_input_type(x, what, ..., arg = arg, call = call)
  }
}
