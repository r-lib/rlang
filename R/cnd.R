#' Create a condition object
#'
#' These constructors make it easy to create subclassed conditions.
#' Conditions are objects that power the error system in R. They can
#' also be used for passing messages to pre-established handlers.
#'
#' `cnd()` creates objects inheriting from `condition`. Conditions
#' created with `error_cnd()`, `warning_cnd()` and `message_cnd()`
#' inherit from `error`, `warning` or `message`.
#'
#' @section Lifecycle:
#'
#' The `.type` and `.msg` arguments have been renamed to `.subclass`
#' and `message`. They are deprecated as of rlang 0.3.0.
#'
#' @param .subclass The condition subclass.
#' @param ... Named data fields stored inside the condition
#'   object. These dots are evaluated with [explicit
#'   splicing][tidy-dots].
#' @param message A default message to inform the user about the
#'   condition when it is signalled.
#' @param trace A `trace` object created by [trace_back()].
#' @param parent A parent condition object created by [abort()].
#' @seealso [cnd_signal()], [with_handlers()].
#'
#' @keywords internal
#' @export
#' @examples
#' # Create a condition inheriting from the s3 type "foo":
#' cnd <- cnd("foo")
#'
#' # Signal the condition to potential handlers. Since this is a bare
#' # condition the signal has no effect if no handlers are set up:
#' cnd_signal(cnd)
#'
#' # When a relevant handler is set up, the signal causes the handler
#' # to be called:
#' with_handlers(cnd_signal(cnd), foo = exiting(function(c) "caught!"))
#'
#' # Handlers can be thrown or executed inplace. See with_handlers()
#' # documentation for more on this.
#'
#' # Signalling an error condition aborts the current computation:
#' err <- error_cnd("foo", message = "I am an error")
#' try(cnd_signal(err))
cnd <- function(.subclass, ..., message = "") {
  if (missing(.subclass)) {
    abort("Bare conditions must be subclassed")
  }
  .Call(rlang_new_condition, .subclass, message, dots_list(...))
}
#' @rdname cnd
#' @export
warning_cnd <- function(.subclass = NULL, ..., message = "") {
  .Call(rlang_new_condition, c(.subclass, "warning"), message, dots_list(...))
}
#' @rdname cnd
#' @export
message_cnd <- function(.subclass = NULL, ..., message = "") {
  .Call(rlang_new_condition, c(.subclass, "message"), message, dots_list(...))
}

#' Is object a condition?
#' @param x An object to test.
#' @keywords internal
#' @export
is_condition <- function(x) {
  inherits(x, "condition")
}

#' What type is a condition?
#'
#' Use `cnd_type()` to check what type a condition is.
#'
#' @param cnd A condition object.
#' @return A string, either `"condition"`, `"message"`, `"warning"`,
#'   `"error"` or `"interrupt"`.
#'
#' @keywords internal
#' @export
#' @examples
#' cnd_type(catch_cnd(abort("Abort!")))
#' cnd_type(catch_cnd(interrupt()))
cnd_type <- function(cnd) {
  .Call(rlang_cnd_type, cnd)
}
