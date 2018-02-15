#' Establish handlers on the stack
#'
#' Condition handlers are functions established on the evaluation
#' stack (see [ctxt_stack()]) that are called by R when a condition is
#' signalled (see [cnd_signal()] and [abort()] for two common signal
#' functions). They come in two types: exiting handlers, which jump
#' out of the signalling context and are transferred to
#' `with_handlers()` before being executed. And inplace handlers,
#' which are executed within the signal functions.
#'
#' An exiting handler is taking charge of the condition. No other
#' handler on the stack gets a chance to handle the condition. The
#' handler is executed and `with_handlers()` returns the return value
#' of that handler. On the other hand, in place handlers do not
#' necessarily take charge. If they return normally, they decline to
#' handle the condition, and R looks for other handlers established on
#' the evaluation stack. Only by jumping to an earlier call frame can
#' an inplace handler take charge of the condition and stop the
#' signalling process. Sometimes, a muffling restart has been
#' established for the purpose of jumping out of the signalling
#' function but not out of the context where the condition was
#' signalled, which allows execution to resume normally. See
#' [rst_muffle()] the `muffle` argument of [inplace()] and the
#' `mufflable` argument of [cnd_signal()].
#'
#' Exiting handlers are established first by `with_handlers()`, and in
#' place handlers are installed in second place. The latter handlers
#' thus take precedence over the former.
#'
#' @inheritParams with_restarts
#' @param .expr An expression to execute in a context where new
#'   handlers are established. The underscored version takes a quoted
#'   expression or a quoted formula.
#' @param ... Named handlers. Handlers should inherit from `exiting`
#'   or `inplace`. See [exiting()] and [inplace()] for constructing
#'   such handlers. Dots are evaluated with [explicit
#'   splicing][tidy-dots].
#' @seealso [exiting()], [inplace()].
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
#' # Exiting handlers jump to with_handlers() before being
#' # executed. Their return value is handed over:
#' handler <- function(c) "handler return value"
#' with_handlers(fn(), foo = exiting(handler))
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
with_handlers <- function(.expr, ...) {
  quo <- enquo(.expr)
  handlers <- dots_list(...)

  inplace <- keep(handlers, inherits, "inplace")
  exiting <- keep(handlers, inherits, "exiting")

  if (length(handlers) > length(exiting) + length(inplace)) {
    abort("all handlers should inherit from `exiting` or `inplace`")
  }
  if (length(exiting)) {
    quo <- quo(tryCatch(!! quo, !!! exiting))
  }
  if (length(inplace)) {
    quo <- quo(withCallingHandlers(!! quo, !!! inplace))
  }

  eval_tidy(quo)
}

#' Create an exiting or in place handler
#'
#' There are two types of condition handlers: exiting handlers, which
#' are thrown to the place where they have been established (e.g.,
#' [with_handlers()]'s evaluation frame), and local handlers, which
#' are executed in place (e.g., where the condition has been
#' signalled). `exiting()` and `inplace()` create handlers suitable
#' for [with_handlers()].
#'
#' A subtle point in the R language is that conditions are not thrown,
#' handlers are. [base::tryCatch()] and [with_handlers()] actually
#' catch handlers rather than conditions. When a critical condition
#' signalled with [base::stop()] or [abort()], R inspects the handler
#' stack and looks for a handler that can deal with the condition. If
#' it finds an exiting handler, it throws it to the function that
#' established it ([with_handlers()]). That is, it interrupts the
#' normal course of evaluation and jumps to `with_handlers()`
#' evaluation frame (see [ctxt_stack()]), and only then and there the
#' handler is called. On the other hand, if R finds an inplace
#' handler, it executes it locally. The inplace handler can choose to
#' handle the condition by jumping out of the frame (see [rst_jump()]
#' or [return_from()]). If it returns locally, it declines to handle
#' the condition which is passed to the next relevant handler on the
#' stack. If no handler is found or is able to deal with the critical
#' condition (by jumping out of the frame), R will then jump out of
#' the faulty evaluation frame to top-level, via the abort restart
#' (see [rst_abort()]).
#'
#' @param handler A handler function that takes a condition as
#'   argument. This is passed to [as_function()] and can thus be a
#'   formula describing a lambda function.
#' @param muffle Whether to muffle the condition after executing an
#'   inplace handler. The signalling function must have established a
#'   muffling restart. Otherwise, an error will be issued.
#' @seealso [with_handlers()] for examples, [restarting()] for another
#'   kind of inplace handler.
#' @export
#' @examples
#' # You can supply a function taking a condition as argument:
#' hnd <- exiting(function(c) cat("handled foo\n"))
#' with_handlers(cnd_signal("foo"), foo = hnd)
#'
#' # Or a lambda-formula where "." is bound to the condition:
#' with_handlers(foo = inplace(~cat("hello", .$attr, "\n")), {
#'   cnd_signal("foo", attr = "there")
#'   "foo"
#' })
exiting <- function(handler) {
  handler <- as_function(handler)
  structure(handler, class = c("exiting", "handler"))
}
#' @rdname exiting
#' @export
inplace <- function(handler, muffle = FALSE) {
  handler <- as_function(handler)
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

#' Create a restarting handler
#'
#' This constructor automates the common task of creating an
#' [inplace()] handler that invokes a restart.
#'
#' Jumping to a restart point from an inplace handler has two
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
#' @param ... Additional arguments passed on the restart
#'   function. These arguments are evaluated only once and
#'   immediately, when creating the restarting handler. Furthermore,
#'   they support [tidy dots][tidy-dots] features.
#' @export
#' @seealso [inplace()] and [exiting()].
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
restarting <- function(.restart, ..., .fields = NULL) {
  stopifnot(is_scalar_character(.restart))
  if (!is_null(.fields)) {
    .fields <- set_names2(.fields)
    stopifnot(is_character(.fields) && is_dictionaryish(.fields))
  }

  args <- dots_list(...)
  handler <- function(c) {
    fields <- set_names(c[.fields], names(.fields))
    rst_args <- c(fields, args)
    do.call("rst_jump", c(list(.restart = .restart), rst_args))
  }

  structure(handler, class = c("restarting", "inplace", "handler"))
}
