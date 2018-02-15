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
#' @param .type The condition subclass.
#' @param ... Named data fields stored inside the condition
#'   object. These dots are evaluated with [explicit
#'   splicing][tidy-dots].
#' @param .msg A default message to inform the user about the
#'   condition when it is signalled.
#' @seealso [cnd_signal()], [with_handlers()].
#' @export
#' @examples
#' # Create a condition inheriting from the s3 type "foo":
#' cnd <- cnd("foo")
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
#' cnd_signal(error_cnd("my_error"))
#'
#' # you need to use stop() to signal a critical condition that should
#' # terminate the program if not handled:
#' # stop(error_cnd("my_error"))
cnd <- function(.type = NULL, ..., .msg = NULL) {
  .Call(rlang_new_condition, .type, dots_list(...), .msg)
}
#' @rdname cnd
#' @export
error_cnd <- function(.type = NULL, ..., .msg = NULL) {
  cnd(c(.type, "error"), ..., .msg = .msg)
}
#' @rdname cnd
#' @export
warning_cnd <- function(.type = NULL, ..., .msg = NULL) {
  cnd(c(.type, "warning"), ..., .msg = .msg)
}
#' @rdname cnd
#' @export
message_cnd <- function(.type = NULL, ..., .msg = NULL) {
  cnd(c(.type, "message"), ..., .msg = .msg)
}

#' Is object a condition?
#' @param x An object to test.
#' @export
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
#' @inheritParams cnd
#' @param .cnd Either a condition object (see [cnd()]), or the
#'   name of a s3 class from which a new condition will be created.
#' @param .msg A string to override the condition's default message.
#' @param .call Whether to display the call of the frame in which the
#'   condition is signalled. If `TRUE`, the call is stored in the
#'   `call` field of the condition object: this field is displayed by
#'   R when an error is issued. If a number `n`, the call is taken
#'   from the nth frame on the [call stack][call_stack]. If `NULL`,
#'   the call is taken from the `.call` field that was supplied to the
#'   condition constructor (e.g. [cnd()]). In all cases the `.call`
#'   field is updated with the actual call.
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
#' cnd <- cnd("foo")
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
#' # cnd_warn() and cnd_inform() signal a condition and display a
#' # warning or message:
#' \dontrun{
#' cnd_inform(cnd)
#' cnd_warn(cnd)
#' }
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
#' # (supplied to cnd()) are displayed. Otherwise, the ones
#' # supplied to cnd_abort() and cnd_signal() take precedence:
#' \dontrun{
#' critical <- cnd("my_error",
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
cnd_signal <- function(.cnd, ..., .msg = NULL, .call = NULL,
                       .mufflable = TRUE) {
  cnd <- cnd_update(.cnd, ..., .msg = .msg, .call = cnd_call(.call), .show_call = .call)
  invisible(.Call(rlang_cnd_signal, cnd, .mufflable))
}
#' @rdname cnd_signal
#' @export
cnd_inform <- function(.cnd, ..., .msg = NULL, .call = NULL,
                     .mufflable = FALSE) {
  cnd <- as_special_cnd(.cnd, "message")
  cnd <- cnd_update(cnd, ..., .msg = .msg, .call = cnd_call(.call), .show_call = .call)
  invisible(.Call(rlang_cnd_inform, cnd, .mufflable))
}
#' @rdname cnd_signal
#' @export
cnd_warn <- function(.cnd, ..., .msg = NULL, .call = NULL,
                     .mufflable = FALSE) {
  cnd <- as_special_cnd(.cnd, "warning")
  cnd <- cnd_update(cnd, ..., .msg = .msg, .call = cnd_call(.call), .show_call = .call)
  invisible(.Call(rlang_cnd_warn, cnd, .mufflable))
}
#' @rdname cnd_signal
#' @export
cnd_abort <- function(.cnd, ..., .msg = NULL, .call = NULL,
                      .mufflable = FALSE) {
  cnd <- as_special_cnd(.cnd, "error")
  cnd <- cnd_update(cnd, ..., .msg = .msg, .call = cnd_call(.call), .show_call = .call)
  invisible(.Call(rlang_cnd_abort, cnd, .mufflable))
}

cnd_call <- function(call) {
  if (is_scalar_logical(call) || is_null(call)) {
    call <- 1
  } else if (!is_scalar_integerish(call)) {
    stop("`call` must be a scalar boolean or number", call. = FALSE)
  }

  caller_frame(call + 1)$expr
}
cnd_update <- function(.cnd, ..., .msg, .call, .show_call) {
  if (is_character(.cnd)) {
    .cnd <- cnd(.cnd, ...)
  } else {
    stopifnot(is_condition(.cnd))
  }

  # Override default field if supplied
  .cnd$message <- .msg %||% .cnd$message %||% ""

  # The `call` field is displayed by stop() and display is controlled
  # by user. If NULL, use the call stored in the condition. If TRUE,
  # use the current call.
  if (is_null(.show_call)) {
    .cnd$call <- .cnd$.call
  } else if (is_true(.show_call) || is_scalar_integerish(.show_call)) {
    .cnd$call <- .call
  } else if (is_false(.show_call)) {
    .cnd$call <- NULL
  } else {
    stop("Internal error: unexpected `.show_call`", call. = FALSE)
  }

  # Record actual call in `.call` in all cases
  .cnd$.call <- .call

  .cnd
}
as_special_cnd <- function(cnd, type) {
  if (is_character(cnd) && !type %in% cnd) {
    return(c(cnd, type))
  }

  if (is_condition(cnd) && !inherits(cnd, type)) {
    classes <- class(cnd)

    pos <- match("condition", classes)
    before <- classes[seq_len(pos - 1)]
    after <- classes[seq.int(pos, length(classes))]

    class(cnd) <- chr(before, type, after)
  }

  cnd
}

# Used in C implementation
muffle <- function(...) NULL


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
#' @param call Whether to display the call. If a number `n`, the call
#'   is taken from the nth frame on the [call stack][call_stack].
#' @export
abort <- function(msg, type = NULL, call = FALSE) {
  cnd <- error_cnd(type, .msg = msg, .call = cnd_call(call))
  if (!is_false(call)) {
    cnd$call <- cnd$.call
  }
  stop(cnd)
}
#' @rdname abort
#' @export
warn <- function(msg, type = NULL, call = FALSE) {
  cnd <- warning_cnd(type, .msg = msg, .call = cnd_call(call))
  if (!is_false(call)) {
    cnd$call <- cnd$.call
  }
  warning(cnd)
}
#' @rdname abort
#' @export
inform <- function(msg, type = NULL, call = FALSE) {
  msg <- paste0(msg, "\n")
  cnd <- message_cnd(type, .msg = msg, .call = cnd_call(call))
  if (!is_false(call)) {
    cnd$call <- cnd$.call
  }
  message(cnd)
}

#' Catch a condition
#'
#' This is a small wrapper around `tryCatch()` that captures any
#' condition signalled while evaluating its argument. It is useful for
#' debugging and unit testing.
#'
#' @param expr Expression to be evaluated with a catch-all condition
#'   handler.
#' @return A condition if any was signalled, `NULL` otherwise.
#' @export
#' @examples
#' catch_cnd(10)
#' catch_cnd(abort("an error"))
#' catch_cnd(cnd_signal("my_condition", .msg = "a condition"))
catch_cnd <- function(expr) {
  tryCatch(condition = identity, {
    force(expr)
    return(NULL)
  })
}
