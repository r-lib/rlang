 #' Signal a condition object
#'
#' @description
#'
#' `cnd_signal()` takes a condition as argument and emits the
#' corresponding signal. The type of signal depends on the class of
#' the condition:
#'
#' * A message is signalled if the condition inherits from
#'   `"message"`. This is equivalent to signalling with [inform()] or
#'   [base::message()].
#'
#' * A warning is signalled if the condition inherits from
#'   `"warning"`. This is equivalent to signalling with [warn()] or
#'   [base::warning()].
#'
#' * An error is signalled if the condition inherits from
#'   `"error"`. This is equivalent to signalling with [abort()] or
#'   [base::stop()].
#'
#' * An interrupt is signalled if the condition inherits from
#'   `"interrupt"`. This is equivalent to signalling with
#'   [interrupt()].
#'
#' @param cnd A condition object (see [cnd()]). If `NULL`,
#'   `cnd_signal()` returns without signalling a condition.
#' @inheritParams args_dots_empty
#' @seealso
#' * [cnd_type()] to determine the type of a condition.
#'
#' * [abort()], [warn()] and [inform()] for creating and signalling
#'   structured R conditions in one go.
#'
#' * [try_fetch()] for establishing condition handlers for
#'   particular condition classes.
#' @export
#' @examples
#' # The type of signal depends on the class. If the condition
#' # inherits from "warning", a warning is issued:
#' cnd <- warning_cnd("my_warning_class", message = "This is a warning")
#' cnd_signal(cnd)
#'
#' # If it inherits from "error", an error is raised:
#' cnd <- error_cnd("my_error_class", message = "This is an error")
#' try(cnd_signal(cnd))
cnd_signal <- function(cnd, ...) {
  check_dots_empty0(...)

  .__signal_frame__. <- TRUE

  if (is_null(cnd)) {
    return(invisible(NULL))
  }

  switch(
    cnd_type(cnd),
    error = {
      if (is_environment(cnd$call)) {
        frame <- cnd$call
        cnd$call <- error_call(cnd$call)
      } else {
        frame <- caller_env()
      }
      if (is_null(cnd$trace)) {
        info <- abort_context(frame, rethrowing = !is_null(cnd$parent))
        with_options(
          "rlang:::visible_bottom" = info$bottom_frame,
          { cnd$trace <- trace_back() }
        )
      }
      signal_abort(cnd)
    },
    warning = warning(cnd),
    message = message(cnd),
    interrupt = interrupt(),
    condition = invisible(withRestarts(
      rlang_muffle = function() NULL,
      signalCondition(cnd)
    ))
  )
}

#' @rdname abort
#' @param .frequency How frequently should the warning or message be
#'   displayed? By default (`"always"`) it is displayed at each
#'   time. If `"regularly"`, it is displayed once every 8 hours. If
#'   `"once"`, it is displayed once per session.
#' @param .frequency_id A unique identifier for the warning or
#'   message. This is used when `.frequency` is supplied to recognise
#'   recurring conditions. This argument must be supplied if
#'   `.frequency` is not set to `"always"`.
#' @export
warn <- function(message = NULL,
                 class = NULL,
                 ...,
                 body = NULL,
                 footer = NULL,
                 parent = NULL,
                 use_cli_format = NULL,
                 .inherit = NULL,
                 .frequency = c("always", "regularly", "once"),
                 .frequency_id = NULL,
                 .subclass = deprecated()) {
  message <- validate_signal_args(message, class, NULL, .subclass, "warn")

  message_info <- cnd_message_info(
    message,
    body,
    footer,
    caller_env(),
    use_cli_format = use_cli_format
  )
  message <- message_info$message
  extra_fields <- message_info$extra_fields
  use_cli_format <- message_info$use_cli_format

  .frequency <- arg_match0(.frequency, c("always", "regularly", "once"))
  if (!needs_signal(.frequency, .frequency_id, warning_freq_env, "rlib_warning_verbosity")) {
    return(invisible(NULL))
  }

  if (!is_null(parent)) {
    # Don't inherit from `parent` by default if chained to a
    # downgraded error
    if (is_null(.inherit)) {
      .inherit <- !inherits(parent, "error")
    }
    extra_fields$rlang <- c(
      extra_fields$rlang,
      list(inherit = .inherit)
    )
  }

  cnd <- warning_cnd(
    class,
    message = message,
    !!!extra_fields,
    use_cli_format = use_cli_format,
    parent = parent,
    ...
  )
  cnd$footer <- c(cnd$footer, message_freq(message, .frequency, "warning"))

  local_long_messages()
  warning(cnd)
}
#' @rdname abort
#' @export
inform <- function(message = NULL,
                   class = NULL,
                   ...,
                   body = NULL,
                   footer = NULL,
                   parent = NULL,
                   use_cli_format = NULL,
                   .inherit = NULL,
                   .file = NULL,
                   .frequency = c("always", "regularly", "once"),
                   .frequency_id = NULL,
                   .subclass = deprecated()) {
  message <- message %||% ""

  validate_signal_args(message, class, NULL, .subclass, "inform")

  message_info <- cnd_message_info(
    message,
    body,
    footer,
    caller_env(),
    use_cli_format = use_cli_format
  )
  message <- message_info$message
  extra_fields <- message_info$extra_fields
  use_cli_format <- message_info$use_cli_format

  .frequency <- arg_match0(.frequency, c("always", "regularly", "once"))
  if (!needs_signal(.frequency, .frequency_id, message_freq_env, "rlib_message_verbosity")) {
    return(invisible(NULL))
  }

  if (!is_null(parent)) {
    # Don't inherit from `parent` by default if chained to a
    # downgraded warning or error
    if (is_null(.inherit)) {
      .inherit <- !inherits(parent, c("warning", "error"))
    }
    extra_fields$rlang <- c(
      extra_fields$rlang,
      list(inherit = .inherit)
    )
  }

  cnd <- message_cnd(
    class,
    message = message,
    !!!extra_fields,
    parent = parent,
    use_cli_format = use_cli_format,
    ...
  )
  cnd$footer <- c(cnd$footer, message_freq(message, .frequency, "message"))

  withRestarts(muffleMessage = function() NULL, {
    signalCondition(cnd)
    msg <- paste0(conditionMessage(cnd), "\n")
    cat(msg, file = .file %||% default_message_file())
  })

  invisible()
}
#' @rdname abort
#' @export
signal <- function(message = "",
                   class,
                   ...,
                   .subclass = deprecated()) {
  validate_signal_args(message, class, NULL, .subclass, "signal")

  message <- .rlang_cli_format_fallback(message)
  cnd <- cnd(class, ..., message = message)
  cnd_signal(cnd)
}

# Increase message length temporarily if it set to the default
# value. The limit can quickly be hit if the message includes a lot of
# ANSI escapes.
local_long_messages <- function(..., frame = caller_env()) {
  if (peek_option("warning.length") == 1000) {
    local_options(warning.length = 8170, .frame = frame)
  }
}

default_message_file <- function() {
  opt <- peek_option("rlang:::message_file")
  if (!is_null(opt)) {
    return(opt)
  }

  if ((is_interactive() || is_rstudio()) &&
      sink.number("output") == 0 &&
      sink.number("message") == 2) {
    stdout()
  } else {
    stderr()
  }
}

is_rstudio <- function() {
  Sys.getenv("RSTUDIO_SESSION_PID") %in% c(Sys.getpid(), getppid())
}

deprecate_subclass <- function(subclass, fn, env = caller_env()) {
  msg <- sprintf(
    "The %s argument of %s has been renamed to %s.",
    format_arg(".subclass"),
    format_fn(fn),
    format_arg("class")
  )
  # 2022-01: Too many packages still use `.subclass`
  # - https://github.com/ropensci/jstor/issues/88
  # - https://github.com/jacob-long/jtools/issues/118
  # - https://github.com/tidyverse/tibble/issues/1015
  # - https://github.com/r-lib/pkgload/issues/188
  # - https://github.com/poissonconsulting/chk/issues/102
  # - https://github.com/burchill/catchr/issues/8
  # - https://github.com/cynkra/dm/issues/743
  # - https://github.com/factset/analyticsapi-engines-r-sdk/issues/13
  # - https://github.com/tidymodels/textrecipes/issues/152
  # - https://github.com/NikKrieger/sociome/issues/14
  # - https://github.com/r-lib/testthat/commit/f09df60dd881530332b252474e9f35c97f8640be
  if (is_true(peek_option("force_subclass_deprecation"))) {
    deprecate_soft(msg)
  }
  env_bind(env, class = subclass)
}

#' Simulate interrupt condition
#'
#' `interrupt()` simulates a user interrupt of the kind that is
#' signalled with `Ctrl-C`. It is currently not possible to create
#' custom interrupt condition objects.
#'
#' @keywords internal
#' @export
interrupt <- function() {
  .Call(ffi_interrupt)
}

validate_signal_args <- function(message,
                                 class,
                                 call,
                                 subclass,
                                 fn,
                                 env = caller_env()) {
  local_error_call("caller")

  if (!is_missing(subclass)) {
    deprecate_subclass(subclass, fn, env)
  }
  check_required(class, call = env)

  if (!is_missing(call)) {
    if (!is_null(call) && !is_environment(call) && !is_call(call)) {
      stop_input_type(call, "a call or environment", arg = "call", call = env)
    }
  }

  if (is_null(message) && is_null(class)) {
    abort("Either `message` or `class` must be supplied.", call = env)
  }

  message <- message %||% ""
  if (is_function(message)) {
    if (!"..." %in% names(formals(message))) {
      abort("`cnd_header()` methods must take `...`.", call = env)
    }
  } else {
    check_character(message, call = env)
  }

  if (!is_null(class)) {
    check_character(class, call = env)
  }

  message
}


warning_freq_env <- new.env(parent = emptyenv())
message_freq_env <- new.env(parent = emptyenv())

needs_signal <- function(frequency,
                         id,
                         env,
                         opt) {
  local_error_call("caller")

  switch(
    peek_verbosity(opt),
    verbose = return(TRUE),
    quiet = return(FALSE),
    default = NULL
  )

  if (is_string(frequency, "always")) {
    return(TRUE)
  }
  # Safe to remove in 2022
  if (is_true(peek_option("rlang:::message_always"))) {
    return(TRUE)
  }

  if (is_null(id)) {
    abort(sprintf(
      "%s must be supplied with %s.",
      format_arg(".frequency_id"),
      format_arg(".frequency")
    ))
  }
  check_name(id, arg = ".frequency")

  sentinel <- env[[id]]
  if (is_null(sentinel)) {
    env_poke(env, id, Sys.time())
    return(TRUE)
  }

  if (is_string(frequency, "once")) {
    return(FALSE)
  }

  if (!inherits(sentinel, "POSIXct")) {
    abort("Expected `POSIXct` value.", .internal = TRUE)
  }

  # Signal every 8 hours
  (Sys.time() - sentinel) > (8 * 60 * 60)
}
peek_verbosity <- function(opt, call = caller_env()) {
  arg_match0(
    peek_option(opt) %||% "default",
    c("default", "verbose", "quiet"),
    opt,
    error_call = call
  )
}

#' @rdname abort
#' @param id The identifying string of the condition that was supplied
#'   as `.frequency_id` to `warn()` or `inform()`.
#' @export
reset_warning_verbosity <- function(id) {
  reset_verbosity(id, "warning")
}
#' @rdname abort
#' @export
reset_message_verbosity <- function(id) {
  reset_verbosity(id, "message")
}
reset_verbosity <- function(id, type = c("message", "warning")) {
  check_name(id)
  type <- arg_match(type)

  env <- switch(
    type,
    message = message_freq_env,
    warning = warning_freq_env
  )
  env[[id]] <- NULL

  invisible(NULL)
}

message_freq <- function(message, frequency, type) {
  if (is_string(frequency, "always")) {
    return(chr())
  }

  if (is_string(frequency, "regularly")) {
    info <- col_silver("This %s is displayed once every 8 hours.")
  } else {
    info <- col_silver("This %s is displayed once per session.")
  }
  sprintf(info, type)
}
