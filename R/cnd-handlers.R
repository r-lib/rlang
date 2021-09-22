#' Try an expression with condition handlers
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `try_catch()` establishes handlers for conditions of a given class
#' (`"error"`, `"warning"`, `"message"`, ...). Handlers are functions
#' that take a condition object as argument and are called when the
#' corresponding condition class has been signalled.
#'
#' A condition handler can:
#'
#' -   *Recover from conditions** with a value. In this case the computation of
#'     `expr` is aborted and the recovery value is returned from
#'     `try_catch()`. Error recovery is useful when you don't want
#'     errors to abruptly interrupt your program but resume at the
#'     catching site instead.
#'
#'     ```
#'     # Recover with the value 0
#'     try_catch(1 + "", error = function(cnd) 0)
#'     ```
#'
#' -   **Rethrow conditions**, e.g. using `abort(msg, parent = cnd)`.
#'     See the `parent` argument of [abort()]. This is typically done to
#'     add information to low-level errors about the high-level context
#'     in which they occurred.
#'
#'     ```
#'     try_catch(1 + "", error = function(cnd) abort("Failed.", parent = cnd))
#'     ```
#'
#' -   **Inspect conditions**, for instance to log data about warnings
#'     or errors. In this case, the handler must return the [zap()]
#'     sentinel to instruct `try_catch()` to ignore (or zap) that
#'     particular handler. The next matching handler is called if any,
#'     and errors bubble up to the user if no handler remains.
#'
#'     ```
#'     log <- NULL
#'     try_catch(1 + "", error = function(cnd) {
#'       log <<- cnd
#'       zap()
#'     })
#'     ```
#'
#' @param expr An R expression.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named condition
#'   handlers. The names specify the condition class for which a
#'   handler will be called.
#'
#' @section Stack overflows:
#'
#' A stack overflow occurs when a program keeps adding to itself until
#' the stack memory (whose size is very limited unlike heap memory) is
#' exhausted.
#'
#' ```
#' # A function that calls itself indefinitely causes stack overflows
#' f <- function() f()
#' f()
#' #> Error: C stack usage  9525680 is too close to the limit
#' ```
#'
#' Because `try_catch()` preserves as much of the running program as
#' possible in order to produce informative backtraces on error
#' rethrows, it is unable to deal with stack overflows. When an
#' overflow occurs, R needs to unwind as much of the program as is
#' possible and can't afford to run any more R code, such as in an
#' error handler.
#'
#' This usually isn't a problem for error rethrowing or logging but
#' might make your program more brittle in case of error recovery. In
#' that case, capture errors with [base::tryCatch()] instead of
#' `try_catch()`.
#'
#' @section Comparison with `tryCatch()`:
#'
#' `try_catch()` generalises `tryCatch()` and `withCallingHandlers()`
#' in a single function. It reproduces the behaviour of both calling
#' and exiting handlers depending the on the return value of the
#' handler. If the handler returns the [zap()] sentinel, it is taken
#' as a calling handler that declines to recover from a condition.
#' Otherwise, it is taken as an exiting handler which returns a value
#' from the catching site.
#'
#' The important difference between `tryCatch()` and `try_catch()` is
#' that the program in `expr` is still fully running when an error
#' handler is called. Because the call stack is preserved, this makes
#' it possible to capture a full backtrace from within the handler,
#' e.g. when rethrowing the error with `abort(parent = cnd)`.
#' Technically, `try_catch()` is more similar to (and implemented on
#' top of) [base::withCallingHandlers()] than `tryCatch().`
#'
#' @export
try_catch <- function(expr, ...) {
  frame <- environment()

  catch <- value <- NULL
  delayedAssign("catch", return(value), frame, frame)

  throw <- function(x) {
    value <<- x
    catch
  }

  .External(ffi_try_catch, frame)
}

handler_call <- quote(function(cnd) {
  out <- handlers[[i]](cnd)
  if (!inherits(out, "rlang_zap")) throw(out)
})


#' Establish handlers on the stack
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' As of rlang 1.0.0, `with_handlers()` is deprecated. Use the base
#' functions or the experimental [try_catch()] function instead.
#'
#' Condition handlers are functions established on the evaluation
#' stack (see [ctxt_stack()]) that are called by R when a condition is
#' signalled (see [cnd_signal()] and [abort()] for two common signal
#' functions). They come in two types:
#'
#' * Exiting handlers aborts all code currently run between
#'   `with_handlers()` and the point where the condition has been
#'   raised. `with_handlers()` passes the return value of the handler
#'   to its caller.
#'
#' * Calling handlers, which are executed from inside the signalling
#'   functions. Their return values are ignored, only their side
#'   effects matters. Valid side effects are writing a log message, or
#'   jumping out of the signalling context by [invoking a
#'   restart][with_restarts] or using [return_from()]. If the raised
#'   condition was an error, this interrupts the aborting process.
#'
#'   If a calling handler returns normally, it effectively declines to
#'   handle the condition and other handlers on the stack (calling or
#'   exiting) are given a chance to handle the condition.
#'
#' Handlers are exiting by default, use [calling()] to create a
#' calling handler.
#'
#' @param .expr An expression to execute in a context where new
#'   handlers are established. The underscored version takes a quoted
#'   expression or a quoted formula.
#' @param ... <[dynamic][dyn-dots]> Named handlers. These should be
#'   functions of one argument, or [formula functions][as_function].
#'   The handlers are considered exiting by default, use [calling()]
#'   to specify a calling handler.
#'
#' @examples
#' # Signal a condition with signal():
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
#'   signal("A foobar condition occurred", "foo")
#'   cat("called?\n")
#' }
#'
#' # Exiting handlers jump to with_handlers() before being
#' # executed. Their return value is handed over:
#' handler <- function(c) "handler return value"
#' with_handlers(fn(), foo = handler)
#'
#' # Calling handlers are called in turn and their return value is
#' # ignored. Returning just means they are declining to take charge of
#' # the condition. However, they can produce side-effects such as
#' # displaying a message:
#' some_handler <- function(c) cat("some handler!\n")
#' other_handler <- function(c) cat("other handler!\n")
#' with_handlers(fn(), foo = calling(some_handler), foo = calling(other_handler))
#'
#' # If a calling handler jumps to an earlier context, it takes
#' # charge of the condition and no other handler gets a chance to
#' # deal with it. The canonical way of transferring control is by
#' # jumping to a restart. See with_restarts() and restarting()
#' # documentation for more on this:
#' exiting_handler <- function(c) rst_jump("rst_foo")
#' fn2 <- function() {
#'   with_restarts(g(), rst_foo = function() "restart value")
#' }
#' with_handlers(fn2(), foo = calling(exiting_handler), foo = calling(other_handler))
#' @keywords internal
#' @export
with_handlers <- function(.expr, ...) {
  handlers <- list2(...)

  is_calling <- map_lgl(handlers, inherits, "rlang_box_calling_handler")
  handlers <- map_if(handlers, is_calling, unbox)
  handlers <- map(handlers, as_function)

  calling <- handlers[is_calling]
  exiting <- handlers[!is_calling]

  expr <- quote(.expr)
  if (length(calling)) {
    expr <- expr(withCallingHandlers(!!expr, !!!calling))
  }
  if (length(exiting)) {
    expr <- expr(tryCatch(!!expr, !!!exiting))
  }

  .External2(ffi_eval, expr, environment())
}

#' @rdname with_handlers
#' @param handler A handler function that takes a condition as
#'   argument. This is passed to [as_function()] and can thus be a
#'   formula describing a lambda function.
#' @export
calling <- function(handler) {
  handler <- as_function(handler)
  new_box(handler, "rlang_box_calling_handler")
}

#' Create a restarting handler
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This constructor automates the common task of creating an
#' [calling()] handler that invokes a restart.
#'
#' Jumping to a restart point from a calling handler has two
#' effects. First, the control flow jumps to wherever the restart was
#' established, and the restart function is called (with `...`, or
#' `.fields` as arguments). Execution resumes from the
#' [with_restarts()] call. Secondly, the transfer of the control flow
#' out of the function that signalled the condition means that the
#' handler has dealt with the condition. Thus the condition will not
#' be passed on to other potential handlers established on the stack.
#'
#' @param .restart The name of a restart.
#' @param .fields A character vector specifying the fields of the
#'   condition that should be passed as arguments to the restart. If
#'   named, the names (except empty names `""`) are used as
#'   argument names for calling the restart function. Otherwise the
#'   the fields themselves are used as argument names.
#' @param ... <[dynamic][dyn-dots]> Additional arguments passed on
#'   the restart function. These arguments are evaluated only once
#'   and immediately, when creating the restarting handler.
#' @seealso [calling()] and [exiting()].
#'
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
#'   with_restarts(signal("A foobar condition occurred", "foo", foo_field = mtcars),
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
#'
#' @keywords internal
#' @export
restarting <- function(.restart, ..., .fields = NULL) {
  stopifnot(is_scalar_character(.restart))
  if (!is_null(.fields)) {
    .fields <- set_names2(.fields)
    stopifnot(is_character(.fields) && is_dictionaryish(.fields))
  }

  args <- list2(...)
  handler <- function(c) {
    fields <- set_names(c[.fields], names(.fields))
    rst_args <- c(fields, args)
    do.call("rst_jump", c(list(.restart = .restart), rst_args))
  }

  calling(handler)
}

#' Catch a condition
#'
#' This is a small wrapper around `tryCatch()` that captures any
#' condition signalled while evaluating its argument. It is useful for
#' situations where you expect a specific condition to be signalled,
#' for debugging, and for unit testing.
#'
#' @param expr Expression to be evaluated with a catching condition
#'   handler.
#' @param classes A character vector of condition classes to catch. By
#'   default, catches all conditions.
#' @return A condition if any was signalled, `NULL` otherwise.
#' @examples
#' catch_cnd(10)
#' catch_cnd(abort("an error"))
#' catch_cnd(signal("my_condition", message = "a condition"))
#' @export
catch_cnd <- function(expr, classes = "condition") {
  stopifnot(is_character(classes))
  handlers <- rep_named(classes, list(identity))

  eval_bare(rlang::expr(
    tryCatch(!!!handlers, {
      force(expr)
      return(NULL)
    })
  ))
}

#' Muffle a condition
#'
#' Unlike [exiting()] handlers, [calling()] handlers must be explicit
#' that they have handled a condition to stop it from propagating to
#' other handlers. Use `cnd_muffle()` within a calling handler (or as
#' a calling handler, see examples) to prevent any other handlers from
#' being called for that condition.
#'
#'
#' @section Mufflable conditions:
#'
#' Most conditions signalled by base R are muffable, although the name
#' of the restart varies. cnd_muffle() will automatically call the
#' correct restart for you. It is compatible with the following
#' conditions:
#'
#' * `warning` and `message` conditions. In this case `cnd_muffle()`
#'   is equivalent to [base::suppressMessages()] and
#'   [base::suppressWarnings()].
#'
#' * Bare conditions signalled with `signal()` or [cnd_signal()]. Note
#'   that conditions signalled with [base::signalCondition()] are not
#'   mufflable.
#'
#' * Interrupts are sometimes signalled with a `resume` restart on
#'   recent R versions. When this is the case, you can muffle the
#'   interrupt with `cnd_muffle()`. Check if a restart is available
#'   with `base::findRestart("resume")`.
#'
#' If you call `cnd_muffle()` with a condition that is not mufflable
#' you will cause a new error to be signalled.
#'
#' * Errors are not mufflable since they are signalled in critical
#'   situations where execution cannot continue safely.
#'
#' * Conditions captured with [base::tryCatch()], [with_handlers()] or
#'   [catch_cnd()] are no longer mufflable. Muffling restarts _must_
#'   be called from a [calling] handler.
#'
#' @param cnd A condition to muffle.
#' @return If `cnd` is mufflable, `cnd_muffle()` jumps to the muffle
#'   restart and doesn't return. Otherwise, it returns `FALSE`.
#'
#' @examples
#' fn <- function() {
#'   inform("Beware!", "my_particular_msg")
#'   inform("On your guard!")
#'   "foobar"
#' }
#'
#' # Let's install a muffling handler for the condition thrown by `fn()`.
#' # This will suppress all `my_particular_wng` warnings but let other
#' # types of warnings go through:
#' with_handlers(fn(),
#'   my_particular_msg = calling(function(cnd) {
#'     inform("Dealt with this particular message")
#'     cnd_muffle(cnd)
#'   })
#' )
#'
#' # Note how execution of `fn()` continued normally after dealing
#' # with that particular message.
#'
#' # cnd_muffle() can also be passed to with_handlers() as a calling
#' # handler:
#' with_handlers(fn(),
#'   my_particular_msg = calling(cnd_muffle)
#' )
#' @keywords internal
#' @export
cnd_muffle <- function(cnd) {
  restart <- switch(cnd_type(cnd),
    message = "muffleMessage",
    warning = "muffleWarning",
    interrupt = "resume",
    "rlang_muffle"
  )

  if (!is_null(findRestart(restart))) {
    invokeRestart(restart)
  }

  FALSE
}

if (getRversion() < "4.0") {
  utils::globalVariables("globalCallingHandlers")
}

drop_global_handlers <- function(...) {
  to_pop <- list(...)
  handlers <- globalCallingHandlers()

  for (i in seq_along(to_pop)) {
    if (loc <- detect_index(handlers, identical, to_pop[[i]])) {
      if (is_string(names(to_pop)[[i]], names(handlers)[[loc]])) {
        handlers[[loc]] <- NULL
      }
    }
  }

  globalCallingHandlers(NULL)
  globalCallingHandlers(handlers)
}
