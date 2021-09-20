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
#' * [with_handlers()] for establishing condition handlers for
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
  validate_cnd_signal_args(cnd, ...)
  if (is_null(cnd)) {
    return(invisible(NULL))
  }

  switch(
    cnd_type(cnd),
    error = {
      if (is_null(cnd$trace)) {
        trace <- trace_back()
        cnd$trace <- trace_trim_context(trace, trace_length(trace))
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
validate_cnd_signal_args <- function(cnd,
                                     ...,
                                     .cnd,
                                     .mufflable,
                                     env = parent.frame()) {
  if (dots_n(...)) {
    abort("`...` must be empty.")
  }
  if (is_character(cnd)) {
    warn_deprecated(paste_line(
      "Creating a condition with `cnd_signal()` is deprecated as of rlang 0.3.0.",
      "Please use `signal()` instead."
    ))
    env$cnd <- cnd(cnd)
  }
  if (!missing(.cnd)) {
    warn_deprecated(paste_line(
      "The `.cnd` argument is deprecated as of rlang 0.3.0.",
      "Please use `cnd` instead."
    ))
    if (is_character(.cnd)) {
      warn_deprecated(paste_line(
        "Creating a condition with `cnd_signal()` is deprecated as of rlang 0.3.0.",
        "Please use `signal()` instead."
      ))
      .cnd <- cnd(.cnd)
    }
    env$cnd <- .cnd
  }
  if (!missing(.mufflable)) {
    warn_deprecated(
      "`.mufflable` is deprecated as of rlang 0.3.0 and no longer has any effect"
    )
  }
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
                 use_cli_format = NULL,
                 .frequency = c("always", "regularly", "once"),
                 .frequency_id = NULL,
                 .subclass = deprecated()) {
  validate_signal_args(.subclass)
  message <- validate_signal_message(message, class)

  message_info <- cnd_message_info(message, caller_env(), use_cli_format = use_cli_format)
  message <- message_info$message
  extra_fields <- message_info$extra_fields

  .frequency <- arg_match0(.frequency, c("always", "regularly", "once"))
  if (!needs_signal(.frequency, .frequency_id, warning_freq_env, "rlib_warning_verbosity")) {
    return(invisible(NULL))
  }

  cnd <- warning_cnd(
    class,
    message = message,
    !!!extra_fields,
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
                   use_cli_format = NULL,
                   .file = NULL,
                   .frequency = c("always", "regularly", "once"),
                   .frequency_id = NULL,
                   .subclass = deprecated()) {
  validate_signal_args(.subclass)
  message <- message %||% ""

  message_info <- cnd_message_info(message, caller_env(), use_cli_format = use_cli_format)
  message <- message_info$message
  extra_fields <- message_info$extra_fields

  .frequency <- arg_match0(.frequency, c("always", "regularly", "once"))
  if (!needs_signal(.frequency, .frequency_id, message_freq_env, "rlib_message_verbosity")) {
    return(invisible(NULL))
  }

  cnd <- message_cnd(
    class,
    message = message,
    !!!extra_fields,
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
signal <- function(message,
                   class,
                   ...,
                   .subclass = deprecated()) {
  validate_signal_args(.subclass)
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

  if (is_interactive() &&
      sink.number("output") == 0 &&
      sink.number("message") == 2) {
    stdout()
  } else {
    stderr()
  }
}

validate_signal_args <- function(subclass, env = caller_env()) {
  local_error_call("caller")
  if (!is_missing(subclass)) {
    deprecate_subclass(subclass, env = env)
  }
}
# TODO! Allow until next major version
deprecate_subclass <- function(subclass, env = caller_env()) {
  local_error_call("caller")
  env_bind(env, class = subclass)
}

#' @rdname abort
#' @export
interrupt <- function() {
  .Call(ffi_interrupt)
}

validate_signal_message <- function(msg, class) {
  local_error_call("caller")

  if (is_null(msg)) {
    if (is_null(class)) {
      abort("Either `message` or `class` must be supplied.")
    }
    msg <- ""
  }

  msg
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
  if (!is_string(id)) {
    abort(sprintf(
      "%s must be a string.",
      format_arg(".frequency")
    ))
  }

  sentinel <- env[[id]]
  if (is_null(sentinel)) {
    env_poke(env, id, Sys.time())
    return(TRUE)
  }

  if (is_string(frequency, "once")) {
    return(FALSE)
  }

  if (!inherits(sentinel, "POSIXct")) {
    stop_internal("needs_signal", "Expected `POSIXct` value.")
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

message_freq <- function(message, frequency, type) {
  if (is_string(frequency, "always")) {
    return(chr())
  }

  if (is_string(frequency, "regularly")) {
    info <- silver("This %s is displayed once every 8 hours.")
  } else {
    info <- silver("This %s is displayed once per session.")
  }
  sprintf(info, type)
}
