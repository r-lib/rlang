#' Create a condition object
#'
#' These constructors make it easy to create subclassed conditions.
#' Conditions are objects that power the error system in R. They can
#' also be used for passing messages to pre-established handlers.
#'
#' `new_cnd()` creates objects inheriting from `condition`. Conditions
#' created with `cnd_error()`, `cnd_warning()` and `cnd_message()`
#' inherit from `error`, `warning` or `message`.
#'
#' @param .type The condition subclass.
#' @param ... Named data fields stored inside the condition
#'   object. These dots are evaluated with [explicit
#'   splicing][dots_list].
#' @param .msg A default message to inform the user about the
#'   condition when it is signalled.
#' @seealso [cnd_signal()], [with_handlers()].
#' @export
#' @examples
#' # Create a condition inheriting from the s3 type "foo":
#' cnd <- new_cnd("foo")
#'
#' # Signal the condition to potential handlers. This has no effect if no
#' # handler is registered to deal with conditions of type "foo":
#' cnd_signal(cnd)
#'
#' # If a relevant handler is on the current evaluation stack, it will be
#' # called by cnd_signal():
#' with_handlers(cnd_signal(cnd), foo = exiting(function(c) "caught!"))
#'
#' # Handlers can be thrown or executed inplace. See with_handlers()
#' # documentation for more on this.
#'
#'
#' # Note that merely signalling a condition inheriting of "error" is
#' # not sufficient to stop a program:
#' cnd_signal(cnd_error("my_error"))
#'
#' # you need to use stop() to signal a critical condition that should
#' # terminate the program if not handled:
#' # stop(cnd_error("my_error"))
new_cnd <- function(.type = NULL, ..., .msg = NULL) {
  data <- dots_list(...)
  if (any(names(data) %in% "message")) {
    stop("Conditions can't have a `message` data field", call. = FALSE)
  }
  if (any(names2(data) == "")) {
    stop("Conditions must have named data fields", call. = FALSE)
  }
  if (!is_null(.msg) && !is_string(.msg)) {
    stop("Condition message must be a string", call. = FALSE)
  }

  cnd <- c(list(message = .msg), data)
  structure(cnd, class = c(.type, "condition"))
}

#' @rdname new_cnd
#' @export
cnd_error <- function(.type = NULL, ..., .msg = NULL) {
  new_cnd(c(.type, "error"), ..., .msg = .msg)
}
#' @rdname new_cnd
#' @export
cnd_warning <- function(.type = NULL, ..., .msg = NULL) {
  new_cnd(c(.type, "warning"), ..., .msg = .msg)
}
#' @rdname new_cnd
#' @export
cnd_message <- function(.type = NULL, ..., .msg = NULL) {
  new_cnd(c(.type, "message"), ..., .msg = .msg)
}

#' Is object a condition?
#' @param x An object to test.
is_condition <- function(x) {
  inherits(x, "condition")
}

#' Signal a condition
#'
#' Signal a condition to handlers that have been established on the
#' stack. Conditions signalled with `cnd_signal()` are assumed to be
#' benign. Control flow can resume normally once the conditions has
#' been signalled (if no handler jumped somewhere else on the
#' evaluation stack). On the other hand, `cnd_abort()` treats the
#' condition as critical and will jump out of the distressed call
#' frame (see [rst_abort()]), unless a handler can deal with the
#' condition.
#'
#' If `.critical` is `FALSE`, this function has no side effects beyond
#' calling handlers. In particular, execution will continue normally
#' after signalling the condition (unless a handler jumped somewhere
#' else via [rst_jump()] or by being [exiting()]). If `.critical` is
#' `TRUE`, the condition is signalled via [base::stop()] and the
#' program will terminate if no handler dealt with the condition by
#' jumping out of the distressed call frame.
#'
#' [inplace()] handlers are called in turn when they decline to handle
#' the condition by returning normally. However, it is sometimes
#' useful for an inplace handler to produce a side effect (signalling
#' another condition, displaying a message, logging something, etc),
#' prevent the condition from being passed to other handlers, and
#' resume execution from the place where the condition was
#' signalled. The easiest way to accomplish this is by jumping to a
#' restart point (see [with_restarts()]) established by the signalling
#' function. If `.mufflable` is `TRUE`, a muffle restart is
#' established. This allows inplace handler to muffle a signalled
#' condition. See [rst_muffle()] to jump to a muffling restart, and
#' the `muffle` argument of [inplace()] for creating a muffling
#' handler.
#'
#' @inheritParams new_cnd
#' @param .cnd Either a condition object (see [new_cnd()]), or the
#'   name of a s3 class from which a new condition will be created.
#' @param .msg A string to override the condition's default message.
#' @param .call Whether to display the call of the frame in which the
#'   condition is signalled. If `TRUE`, the call is stored in the
#'   `call` field of the condition object: this field is displayed by
#'   R when an error is issued. The call information is also stored in
#'   the `.call` field in all cases.
#' @param .mufflable Whether to signal the condition with a muffling
#'   restart. This is useful to let [inplace()] handlers muffle a
#'   condition. It stops the condition from being passed to other
#'   handlers when the inplace handler did not jump elsewhere. `TRUE`
#'   by default for benign conditions, but `FALSE` for critical ones,
#'   since in those cases execution should probably not be allowed to
#'   continue normally.
#' @seealso [abort()], [warn()] and [inform()] for signalling typical
#'   R conditions. See [with_handlers()] for establishing condition
#'   handlers.
#' @export
#' @examples
#' # Creating a condition of type "foo"
#' cnd <- new_cnd("foo")
#'
#' # If no handler capable of dealing with "foo" is established on the
#' # stack, signalling the condition has no effect:
#' cnd_signal(cnd)
#'
#' # To learn more about establishing condition handlers, see
#' # documentation for with_handlers(), exiting() and inplace():
#' with_handlers(cnd_signal(cnd),
#'   foo = inplace(function(c) cat("side effect!\n"))
#' )
#'
#'
#' # By default, cnd_signal() creates a muffling restart which allows
#' # inplace handlers to prevent a condition from being passed on to
#' # other handlers and to resume execution:
#' undesirable_handler <- inplace(function(c) cat("please don't call me\n"))
#' muffling_handler <- inplace(function(c) {
#'   cat("muffling foo...\n")
#'   rst_muffle(c)
#' })
#'
#' with_handlers(foo = undesirable_handler,
#'   with_handlers(foo = muffling_handler, {
#'     cnd_signal("foo")
#'     "return value"
#'   }))
#'
#'
#' # You can signal a critical condition with cnd_abort(). Unlike
#' # cnd_signal() which has no side effect besides signalling the
#' # condition, cnd_abort() makes the program terminate with an error
#' # unless a handler can deal with the condition:
#' \dontrun{
#' cnd_abort(cnd)
#' }
#'
#' # If you don't specify a .msg or .call, the default message/call
#' # (supplied to new_cnd()) are displayed. Otherwise, the ones
#' # supplied to cnd_abort() and cnd_signal() take precedence:
#' \dontrun{
#' critical <- new_cnd("my_error",
#'   .msg = "default 'my_error' msg",
#'   .call = quote(default(call))
#' )
#' cnd_abort(critical)
#' cnd_abort(critical, .msg = "overridden msg")
#'
#' fn <- function(...) {
#'   cnd_abort(critical, .call = TRUE)
#' }
#' fn(arg = foo(bar))
#' }
#'
#' # Note that by default a condition signalled with cnd_abort() does
#' # not have a muffle restart. That is because in most cases,
#' # execution should not continue after signalling a critical
#' # condition.
cnd_signal <- function(.cnd, ..., .msg = NULL, .call = FALSE,
                       .mufflable = TRUE) {
  cnd <- make_cnd(.cnd, ..., .msg = .msg, .call = sys.call(-1), .show_call = .call)
  cnd_signal_(cnd, base::signalCondition, .mufflable)
}
#' @rdname cnd_signal
#' @export
cnd_abort <- function(.cnd, ..., .msg = NULL, .call = FALSE,
                      .mufflable = FALSE) {
  cnd <- make_cnd(.cnd, ..., .msg = .msg, .call = sys.call(-1), .show_call = .call)
  cnd_signal_(cnd, base::stop, .mufflable)
}

make_cnd <- function(.cnd, ..., .msg, .call, .show_call) {
  if (is_scalar_character(.cnd)) {
    .cnd <- new_cnd(.cnd, ...)
  } else {
    stopifnot(is_condition(.cnd))
  }

  # Override default field if supplied
  .cnd$message <- .msg %||% .cnd$message %||% ""

  # The `call` field is displayed by stop().
  # But record call in `.call` in all cases.
  .cnd$.call <- .call
  if (.show_call) {
    .cnd$call <- .cnd$.call
  }

  .cnd
}
cnd_signal_ <- function(cnd, signal, mufflable) {
  if (mufflable) {
    class(cnd) <- c("mufflable", class(cnd))
    withRestarts(signal(cnd), muffle = function(...) NULL)
  } else {
    signal(cnd)
  }
}

#' Signal an error, warning, or message
#'
#' These functions are equivalent to base functions [base::stop()],
#' [base::warning()] and [base::message()], but the `type` argument
#' makes it easy to create subclassed conditions. They also don't
#' include call information by default. This saves you from typing
#' `call. = FALSE` to make error messages cleaner within package
#' functions.
#'
#' Like `stop()` and [cnd_abort()], `abort()` signals a critical
#' condition and interrupts execution by jumping to top level (see
#' [rst_abort()]). Only a handler of the relevant type can prevent
#' this jump by making another jump to a different target on the stack
#' (see [with_handlers()]).
#'
#' `warn()` and `inform()` both have the side effect of displaying a
#' message. These messages will not be displayed if a handler
#' transfers control. Transfer can be achieved by establishing an
#' exiting handler that transfers control to [with_handlers()]). In
#' this case, the current function stops and execution resumes at the
#' point where handlers were established.
#'
#' Since it is often desirable to continue normally after a message or
#' warning, both `warn()` and `inform()` (and their base R equivalent)
#' establish a muffle restart where handlers can jump to prevent the
#' message from being displayed. Execution resumes normally after
#' that. See [rst_muffle()] to jump to a muffling restart, and the
#' `muffle` argument of [inplace()] for creating a muffling handler.
#'
#' @param msg A message to display.
#' @param type Subclass of the condition to signal.
#' @param call Whether to display the call.
#'
#' @export
abort <- function(msg, type = NULL, call = FALSE) {
  cnd <- cnd_error(type, .msg = msg, .call = sys.call(-1))
  if (call) {
    cnd$call <- cnd$.call
  }
  stop(cnd)
}
#' @rdname abort
#' @export
warn <- function(msg, type = NULL, call = FALSE) {
  cnd <- cnd_warning(type, .msg = msg, .call = sys.call(-1))
  if (call) {
    cnd$call <- cnd$.call
  }
  warning(cnd)
}
#' @rdname abort
#' @export
inform <- function(msg, type = NULL, call = FALSE) {
  msg <- paste0(msg, "\n")
  cnd <- cnd_message(type, .msg = msg, .call = sys.call(-1))
  if (call) {
    cnd$call <- cnd$.call
  }
  message(cnd)
}
