#' Signal a condition object
#'
#' @description
#'
#' The type of signal depends on the class of the condition:
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
#' Use [cnd_type()] to determine the type of a condition.
#'
#'
#' @section Lifecycle:
#'
#' * `.cnd` has been renamed to `cnd` and is deprecated as of rlang 0.3.0.
#'
#' * The `.mufflable` argument is deprecated as of rlang 0.3.0 and no
#'   longer has any effect. Non-critical conditions are always
#'   signalled with a muffle restart.
#'
#' * Creating a condition object with [cnd_signal()] is deprecated as
#'   of rlang 0.3.0. Please use [signal()] instead.
#'
#' @param cnd A condition object (see [cnd()]).
#' @param .cnd,.mufflable These arguments are deprecated.
#' @seealso [abort()], [warn()] and [inform()] for creating and
#'   signalling structured R conditions. See [with_handlers()] for
#'   establishing condition handlers.
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
cnd_signal <- function(cnd, .cnd, .mufflable) {
  validate_cnd_signal_args(cnd, .cnd, .mufflable)
  if (inherits(cnd, "rlang_error") && is_null(cnd$trace)) {
    trace <- trace_back()
    cnd$trace <- trace_trim_context(trace, trace_length(trace))
    signal_abort(cnd)
  } else {
    invisible(.Call(rlang_cnd_signal, cnd))
  }
}
validate_cnd_signal_args <- function(cnd, .cnd, .mufflable,
                                     env = parent.frame()) {
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
                 .frequency = c("always", "regularly", "once"),
                 .frequency_id = NULL,
                 .subclass) {
  validate_signal_args(.subclass)

  message <- validate_signal_message(message, class)
  message <- collapse_cnd_message(message)

  .frequency <- arg_match(.frequency, c("always", "regularly", "once"))

  if (needs_signal(.frequency, .frequency_id, warning_freq_env)) {
    message <- add_message_freq(message, .frequency, "warning")
  } else {
    return(invisible(NULL))
  }

  cnd <- warning_cnd(class, ..., message = message)
  warning(cnd)
}
#' @rdname abort
#' @param .file Where the message is printed. This should be a
#'   connection or character string which will be passed to [cat()].
#'
#'   By default, `inform()` prints to standard output in interactive
#'   sessions and standard error otherwise. This way IDEs can treat
#'   messages distinctly from warnings and errors, and R scripts can
#'   still filter out the messages easily by redirecting `stderr`.
#' @export
inform <- function(message = NULL,
                   class = NULL,
                   ...,
                   .file = NULL,
                   .frequency = c("always", "regularly", "once"),
                   .frequency_id = NULL,
                   .subclass) {
  validate_signal_args(.subclass)

  message <- message %||% ""
  message <- collapse_cnd_message(message)
  message <- paste0(message, "\n")

  .frequency <- arg_match(.frequency, c("always", "regularly", "once"))

  if (needs_signal(.frequency, .frequency_id, message_freq_env)) {
    message <- add_message_freq(message, .frequency, "message")
  } else {
    return(invisible(NULL))
  }

  cnd <- message_cnd(class, ..., message = message)

  withRestarts(muffleMessage = function() NULL, {
    signalCondition(cnd)

    .file <- .file %||% if (is_interactive()) stdout() else stderr()
    cat(message, file = .file)
  })

  invisible()
}
#' @rdname abort
#' @export
signal <- function(message, class, ..., .subclass) {
  if (!missing(.subclass)) {
    deprecate_subclass(.subclass)
  }
  message <- collapse_cnd_message(message)
  cnd <- cnd(class, ..., message = message)
  cnd_signal(cnd)
}

validate_signal_args <- function(subclass, env = caller_env()) {
  if (!missing(subclass)) {
    deprecate_subclass(subclass, env = env)
  }
}
# Allow until next major version
deprecate_subclass <- function(subclass, env = caller_env()) {
  env_bind(env, class = subclass)
}

#' @rdname abort
#' @export
interrupt <- function() {
  .Call(rlang_interrupt)
}

validate_signal_message <- function(msg, class) {
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

needs_signal <- function(frequency, id, env) {
  if (is_string(frequency, "always")) {
    return(TRUE)
  }
  if (is_true(peek_option("rlang:::message_always"))) {
    return(TRUE)
  }

  if (is_null(id)) {
    abort("`.frequency_id` should be supplied with `.frequency`.")
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

add_message_freq <- function(message, frequency, type) {
  if (is_string(frequency, "always")) {
    return(message)
  }

  if (is_string(frequency, "regularly")) {
    info <- silver("This %s is displayed once every 8 hours.")
  } else {
    info <- silver("This %s is displayed once per session.")
  }
  info <- sprintf(info, type)

  paste_line(message, info)
}
