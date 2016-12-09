
# conditions ---------------------------------------------------------

#' Create a condition object.
#'
#' These constructors make it easy to create subclassed conditions.
#' Conditions are objects that power the error system in R. They can
#' also be used for passing messages to pre-established handlers. See
#' \code{vignette("conditions")} for more information on how to use
#' the condition system.
#'
#' \code{cnd_new()} creates objects inheriting from
#' \code{condition}. Conditions created with \code{cnd_error()},
#' \code{cnd_warning()} and \code{cnd_message()} inherit from
#' \code{error}, \code{warning} or \code{message}.
#'
#' @param .type The condition subclass.
#' @param ... Named data fields stored inside the condition object.
#' @param .msg A default message to inform the user about the
#'   condition when it is signalled.
#' @seealso \code{\link{cnd_signal}()}, \code{\link{with_handlers}()}.
#' @export
#' @examples
#' # Create a condition inheriting from the s3 type "foo":
#' cnd <- cnd_new("foo")
#'
#' # Signal the condition to potential handlers. This has no effect if no
#' # handler is registered to deal with conditions of type "foo":
#' cnd_signal(cnd)
#'
#' # If a relevant handler is on the current evaluation stack, it will be
#' # called by cnd_signal():
#' with_handlers(cnd_signal(cnd), foo = thrown(function(c) "caught!"))
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
cnd_new <- function(.type = NULL, ..., .msg = NULL) {
  data <- list(...)
  if (any(names(data) %in% "message")) {
    stop("conditions cannot have a `message` data field", call. = FALSE)
  }
  if (any(names2(data) == "")) {
    stop("conditions must have named data fields", call. = FALSE)
  }

  cnd <- c(list(message = .msg), data)
  structure(cnd, class = c(.type, "condition"))
}

#' @rdname cnd_new
#' @export
cnd_error <- function(.type = NULL, ..., .msg = NULL) {
  cnd_new(c(.type, "error"), ..., .msg = .msg)
}
#' @rdname cnd_new
#' @export
cnd_warning <- function(.type = NULL, ..., .msg = NULL) {
  cnd_new(c(.type, "warning"), ..., .msg = .msg)
}
#' @rdname cnd_new
#' @export
cnd_message <- function(.type = NULL, ..., .msg = NULL) {
  cnd_new(c(.type, "message"), ..., .msg = .msg)
}

#' Is object a condition?
#' @param x An object to test.
is_condition <- function(x) {
  inherits(x, "condition")
}

#' Signal a condition.
#'
#' Signal a condition to handlers that have been established on the
#' stack. Conditions signalled with \code{cnd_signal()} are assumed to
#' be benign. Control flow can resume normally once the conditions has
#' been signalled (if no handler jumped somewhere else on the
#' evaluation stack). On the other hand, \code{cnd_abort()} treats the
#' condition as critical and will jump out of the distressed call
#' frame (see \code{\link{rst_abort}()}), unless a handler can deal
#' with the condition.
#'
#' If \code{.critical} is \code{FALSE}, this function has no side
#' effects beyond calling handlers. In particular, execution will
#' continue normally after signalling the condition (unless a handler
#' jumped somewhere else via \code{\link{rst_jump}()} or by being
#' \code{\link{thrown}()}). If \code{.critical} is \code{TRUE}, the
#' condition is signalled via \code{\link[base]{stop}()} and the
#' program will terminate if no handler dealt with the condition by
#' jumping out of the distressed call frame.
#'
#' \code{\link{inplace}()} handlers are called in turn when they
#' decline to handle the condition by returning normally. However, it
#' is sometimes useful for an inplace handler to produce a side effect
#' (signalling another condition, displaying a message, logging
#' something, etc), prevent the condition from being passed to other
#' handlers, and resume execution from the place where the condition
#' was signalled. The easiest way to accomplish this is by jumping to
#' a restart point (see \code{\link{with_restarts}()}) established by
#' the signalling function. If \code{.mufflable} is \code{TRUE}, a
#' muffle restart is established. This allows inplace handler to
#' muffle a signalled condition. See \code{\link{rst_muffle}()} to
#' jump to a muffling restart, and the \code{muffle} argument of
#' \code{\link{inplace}()} for creating a muffling handler.
#'
#' @inheritParams cnd_new
#' @param .cnd Either a condition object (see
#'   \code{\link{cnd_new}()}), or the name of a s3 class from which a
#'   new condition will be created.
#' @param .msg A string to override the condition's default message.
#' @param .call Whether to display the call of the frame in which the
#'   condition is signalled. If \code{TRUE}, the call is stored in the
#'   \code{call} field of the condition object: this field is
#'   displayed by R when an error is issued. The call information is
#'   also stored in the \code{.call} field in all cases.
#' @param .mufflable Whether to signal the condition with a muffling
#'   restart. This is useful to let \code{\link{inplace}()} handlers
#'   muffle a condition. It stops the condition from being passed to
#'   other handlers when the inplace handler did not jump
#'   elsewhere. \code{TRUE} by default for benign conditions, but
#'   \code{FALSE} for critical ones, since in those cases execution
#'   should probably not be allowed to continue normally.
#' @seealso \code{\link{abort}()}, \code{\link{warn}()} and
#'   \code{\link{inform}()} for signalling typical R conditions. See
#'   \code{\link{with_handlers}()} for establishing condition
#'   handlers.
#' @export
#' @examples
#' # Creating a condition of type "foo"
#' cnd <- cnd_new("foo")
#'
#' # If no handler capable of dealing with "foo" is established on the
#' # stack, signalling the condition has no effect:
#' cnd_signal(cnd)
#'
#' # To learn more about establishing condition handlers, see
#' # documentation for with_handlers(), thrown() and inplace():
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
#' # (supplied to cnd_new()) are displayed. Otherwise, the ones
#' # supplied to cnd_abort() and cnd_signal() take precedence:
#' \dontrun{
#' critical <- cnd_new("my_error",
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
    .cnd <- cnd_new(.cnd, ...)
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

#' Signal an error, warning, or message.
#'
#' These functions are equivalent to base functions
#' \code{\link[base]{stop}()}, \code{\link[base]{warning}()} and
#' \code{\link[base]{message}()}, but the \code{type} argument makes
#' it easy to create subclassed conditions. They also don't include
#' call information by default. This saves you from typing
#' \code{call. = FALSE} to make error messages cleaner within package
#' functions.
#'
#' Like \code{stop()} and \code{\link{cnd_abort}()}, \code{abort()}
#' signals a critical condition and interrupts execution by jumping to
#' top level (see \code{\link{rst_abort}()}). Only a handler of the
#' relevant type can prevent this jump by making another jump to a
#' different target on the stack (see \code{\link{with_handlers}()}).
#'
#' \code{warn()} and \code{inform()} both have the side effect of
#' displaying a message. These messages will not be displayed if a
#' handler transfers control. Transfer can be achieved by establishing
#' a thrown handler that transfers control to
#' \code{\link{with_handlers}()}). In this case, the current function
#' stops and execution resumes at the point where handlers were
#' established.
#'
#' Since it is often desirable to continue normally after a message or
#' warning, both \code{warn()} and \code{inform()} (and their base R
#' equivalent) establish a muffle restart where handlers can jump to
#' prevent the message from being displayed. Execution resumes
#' normally after that. See \code{\link{rst_muffle}()} to jump to a
#' muffling restart, and the \code{muffle} argument of
#' \code{\link{inplace}()} for creating a muffling handler.
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
  cnd <- cnd_message(type, .msg = msg, .call = sys.call(-1))
  if (call) {
    cnd$call <- cnd$.call
  }
  message(cnd)
}


# handlers -----------------------------------------------------------

#' Create a thrown or in place handler.
#'
#' There are two types of condition handlers: exiting handlers, which
#' are thrown to the place where they have been established (e.g.,
#' \code{\link{with_handlers}()}'s evaluation frame), and local
#' handlers, which are executed in place (e.g., where the condition
#' has been signalled). \code{thrown()} and \code{inplace()} create
#' handlers suitable for \code{\link{with_handlers}()}.
#'
#' A subtle point in the R language is that conditions are not thrown,
#' handlers are. \code{\link[base]{tryCatch}()} and
#' \code{\link{with_handlers}()} actually catch handlers rather than
#' conditions. When a critical condition signalled with
#' \code{\link[base]{stop}()} or \code{\link{abort}()}, R inspects the
#' handler stack and looks for a handler that can deal with the
#' condition. If it finds a throwable handler, it throws it to the
#' function that established it (\code{\link{with_handlers}()}). If R
#' finds an inplace handler, it executes it locally. Only if no
#' handler can deal with the critical condition will R jump to
#' top-level (see \code{\link{rst_abort}()}).
#'
#' @param handler A handler function that takes a condition as
#'   argument.
#' @param muffle Whether to muffle the condition after executing an
#'   inplace handler. The signalling function must have established a
#'   muffling restart. Otherwise, an error will be issued.
#' @seealso \code{\link{with_handlers}()} for examples,
#'   \code{\link{restarting}()} for another kind of inplace handler.
#' @export
thrown <- function(handler) {
  structure(handler, class = c("thrown", "handler"))
}
#' @rdname thrown
#' @export
inplace <- function(handler, muffle = FALSE) {
  if (muffle) {
    handler_ <- function(c) {
      handler(c)
      rst_muffle(c)
    }
  } else {
    handler_ <- handler
  }
  structure(handler_, class = c("inplace", "handler"))
}

#' Create a restarting handler.
#'
#' This constructor automates the common task of creating an
#' \code{\link{inplace}()} handler that invokes a restart.
#'
#' Jumping to a restart point from an inplace handler has two
#' effects. First, the control flow jumps to wherever the restart was
#' established, and the restart function is called (with \code{...},
#' \code{.args} or \code{.fields} as arguments). Execution resumes
#' from the \code{\link{with_restarts}()} call. Secondly, the transfer
#' of the control flow out of the function that signalled the
#' condition means that the handler has dealt with the condition. Thus
#' the condition will not be passed on to other potential handlers
#' established on the stack.
#'
#' @param .restart The name of a restart.
#' @param .fields A character vector specifying the fields of the
#'   condition that should be passed as arguments to the restart. If
#'   named, the names (except empty names \code{""}) are used as
#'   argument names for calling the restart function. Otherwise the
#'   the fields themselves are used as argument names.
#' @param ...,.args Additional arguments passed on the restart
#'   function. These arguments are evaluated only once and
#'   immediately, when creating the restarting handler.
#'
#' @export
#' @seealso \code{\link{inplace}()} and \code{\link{thrown}()}.
#' @examples
#' # This is a restart that takes a data frame and names as arguments
#' rst_bar <- function(df, nms) {
#'   stats::setNames(df, nms)
#' }
#'
#' # This restart is simpler and does not take arguments
#' rst_baz <- function() "baz"
#'
#' # Signalling a condition parameterised with a data frame
#' fn <- function() {
#'   with_restarts(cnd_signal("foo", foo_field = mtcars),
#'     rst_bar = rst_bar,
#'     rst_baz = rst_baz
#'   )
#' }
#'
#' # Creating a restarting handler that passes arguments `nms` and
#' # `df`, the latter taken from a data field of the condition object
#' restart_bar <- restarting("rst_bar",
#'   nms = LETTERS[1:11], .fields = c(df = "foo_field")
#' )
#'
#' # The restarting handlers jumps to `rst_bar` when `foo` is signalled:
#' with_handlers(fn(), foo = restart_bar)
#'
#' # The restarting() constructor is especially nice to use with
#' # restarts that do not need arguments:
#' with_handlers(fn(), foo = restarting("rst_baz"))
restarting <- function(.restart, ..., .fields = NULL, .args = list()) {
  stopifnot(is_scalar_character(.restart))

  if (!is_null(.fields)) {
    .fields <- set_names2(.fields)
    stopifnot(is_character(.fields) && is_dict(.fields))
  }
  args <- c(list(...), .args)

  handler <- function(c) {
    fields <- set_names(c[.fields], names(.fields))
    rst_args <- c(fields, args)
    do.call("rst_jump", c(list(.restart = .restart), rst_args))
  }

  structure(handler, class = c("restarting", "inplace", "handler"))
}


#' Establish handlers on the stack.
#'
#' Condition handlers are functions established on the evaluation
#' stack (see \code{\link{eval_stack}()}) that are called by R when a
#' condition is signalled (see \code{\link{cnd_signal}()} and
#' \code{\link{abort}()} for two common signal functions). They come
#' in two types: thrown handlers, which jump out of the signalling
#' context and are transferred to \code{with_handlers()} before being
#' executed. And inplace handlers, which are executed within the
#' signal functions.
#'
#' A thrown handler is taking charge of the condition. No other
#' handler on the stack gets a chance to handle the condition. The
#' handler is executed and \code{with_handlers()} returns the return
#' value of that handler. On the other hand, in place handlers do not
#' necessarily take charge. If they return normally, they decline to
#' handle the condition, and R looks for other handlers established on
#' the evaluation stack. Only by jumping to an earlier call frame can
#' an inplace handler take charge of the condition and stop the
#' signalling process. Sometimes, a muffling restart has been
#' established for the purpose of jumping out of the signalling
#' function but not out of the context where the condition was
#' signalled, which allows execution to resume normally. See
#' \code{\link{rst_muffle}()} the \code{muffle} argument of
#' \code{\link{inplace}()} and the \code{mufflable} argument of
#' \code{\link{cnd_signal}()}.
#'
#' Thrown handlers are established first by \code{with_handlers()},
#' and in place handlers are only installed after that. The latter
#' handlers thus take precedence over the former.
#'
#' @inheritParams with_restarts
#' @param .expr An expression to execute in a context where new
#'   handlers are established. The underscored version takes a quoted
#'   expression or a quoted formula.
#' @param ...,.handlers Named handlers. Handlers should inherit from
#'   \code{thrown} or \code{inplace}. See \code{\link{thrown}()} and
#'   \code{\link{inplace}()} for constructing such handlers.
#' @seealso \code{\link{thrown}()}, \code{\link{inplace}()}.
#' @export
#' @examples
#' # Signal a condition with cnd_signal():
#' fn <- function() {
#'   g()
#'   cat("called?\n")
#'   "fn() return value"
#' }
#' g <- function() {
#'   h()
#'   cat("called?\n")
#' }
#' h <- function() {
#'   cnd_signal("foo")
#'   cat("called?\n")
#' }
#'
#' # Thrown handlers jump to with_handlers() before being
#' # executed. Their return value is handed over:
#' handler <- function(c) "handler return value"
#' with_handlers(fn(), foo = thrown(handler))
#'
#' # In place handlers are called in turn and their return value is
#' # ignored. Returning just means they are declining to take charge of
#' # the condition. However, they can produce side-effects such as
#' # displaying a message:
#' some_handler <- function(c) cat("some handler!\n")
#' other_handler <- function(c) cat("other handler!\n")
#' with_handlers(fn(), foo = inplace(some_handler), foo = inplace(other_handler))
#'
#' # If an in place handler jumps to an earlier context, it takes
#' # charge of the condition and no other handler gets a chance to
#' # deal with it. The canonical way of transferring control is by
#' # jumping to a restart. See with_restarts() and restarting()
#' # documentation for more on this:
#' exiting_handler <- function(c) rst_jump("rst_foo")
#' fn2 <- function() {
#'   with_restarts(g(), rst_foo = function() "restart value")
#' }
#' with_handlers(fn2(), foo = inplace(exiting_handler), foo = inplace(other_handler))
with_handlers <- function(.expr, ..., .handlers = list()) {
  handlers <- c(list(...), .handlers)
  with_handlers_(f_capture(.expr), handlers)
}
#' @rdname with_handlers
#' @export
with_handlers_ <- function(.expr, .handlers = list(), .env = NULL) {
  f <- as_quoted_f(.expr, .env)

  inplace <- keep(.handlers, inherits, "inplace")
  thrown <- keep(.handlers, inherits, "thrown")

  if (length(.handlers) > length(thrown) + length(inplace)) {
    abort("all handlers should inherit from `thrown` or `inplace`")
  }

  f <- interp_handlers(f, inplace = inplace, thrown = thrown)
  f_eval(f)
}

interp_handlers <- function(f, inplace, thrown) {
  if (length(thrown)) {
    f <- env_set(f_interp(~tryCatch(uq(f), uqs(thrown))), f)
  }
  if (length(inplace)) {
    f <- env_set(f_interp(~withCallingHandlers(uq(f), uqs(inplace))), f)
  }
  f
}
