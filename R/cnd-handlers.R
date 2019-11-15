#' Establish handlers on the stack
#'
#' @description
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
#' @inheritParams with_restarts
#' @param .expr An expression to execute in a context where new
#'   handlers are established. The underscored version takes a quoted
#'   expression or a quoted formula.
#' @param ... <[dynamic][dyn-dots]> Named handlers. These should be
#'   functions of one argument, or [formula functions][as_function].
#'   The handlers are considered exiting by default, use [calling()]
#'   to specify a calling handler.
#'
#' @section Life cycle: `exiting()` is soft-deprecated as of rlang
#'   0.4.0 because [with_handlers()] now treats handlers as exiting by
#'   default.
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

  .Call(rlang_eval, expr, environment())
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
#' @keywords internal
#' @export
#' @seealso [calling()] and [exiting()].
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
#' @export
#' @examples
#' catch_cnd(10)
#' catch_cnd(abort("an error"))
#' catch_cnd(cnd_signal("my_condition", .msg = "a condition"))
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
#'
#' @keywords internal
#' @export
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

  abort("Can't find a muffling restart")
}
