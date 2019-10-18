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
#' @export
warn <- function(message, .subclass = NULL, ..., call = NULL, msg, type) {
  validate_signal_args(msg, type, call)

  message <- collapse_cnd_message(message)
  cnd <- warning_cnd(.subclass, ..., message = message)
  warning(cnd)
}
#' @rdname abort
#' @export
inform <- function(message, .subclass = NULL, ..., call = NULL, msg, type) {
  validate_signal_args(msg, type, call)

  message <- collapse_cnd_message(message)
  message <- paste0(message, "\n")
  cnd <- message_cnd(.subclass, ..., message = message)
  message(cnd)
}
#' @rdname abort
#' @export
signal <- function(message, .subclass, ...) {
  message <- collapse_cnd_message(message)
  cnd <- cnd(.subclass, ..., message = message)
  cnd_signal(cnd)
}
validate_signal_args <- function(msg, type, call) {
  if (!missing(msg)) {
    stop_defunct("`msg` has been renamed to `message` and is deprecated as of rlang 0.3.0")
  }
  if (!missing(type)) {
    stop_defunct("`type` has been renamed to `.subclass` and is deprecated as of rlang 0.3.0")
  }
  if (!is_null(call)) {
    stop_defunct("`call` is deprecated as of rlang 0.3.0")
  }
}
#' @rdname abort
#' @export
interrupt <- function() {
  .Call(rlang_interrupt)
}
