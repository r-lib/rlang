#' Establish a restart point on the stack
#'
#' Restart points are named functions that are established with
#' `with_restarts()`. Once established, you can interrupt the normal
#' execution of R code, jump to the restart, and resume execution from
#' there. Each restart is established along with a restart function
#' that is executed after the jump and that provides a return value
#' from the establishing point (i.e., a return value for
#' `with_restarts()`).
#'
#' Restarts are not the only way of jumping to a previous call frame
#' (see [return_from()] or [return_to()]). However, they have the advantage of
#' being callable by name once established.
#'
#' @param .expr An expression to execute with new restarts established
#'   on the stack. This argument is passed by expression and supports
#'   [unquoting][quasiquotation]. It is evaluated in a context where
#'   restarts are established.
#' @param ... Named restart functions. The name is taken as the
#'   restart name and the function is executed after the jump. These
#'   dots are evaluated with [explicit splicing][dots_list].
#' @seealso [return_from()] and [return_to()] for a more flexible way
#'   of performing a non-local jump to an arbitrary call frame.
#' @export
#' @examples
#' # Restarts are not the only way to jump to a previous frame, but
#' # they have the advantage of being callable by name:
#' fn <- function() with_restarts(g(), my_restart = function() "returned")
#' g <- function() h()
#' h <- function() { rst_jump("my_restart"); "not returned" }
#' fn()
#'
#' # Whereas a non-local return requires to manually pass the calling
#' # frame to the return function:
#' fn <- function() g(get_env())
#' g <- function(env) h(env)
#' h <- function(env) { return_from(env, "returned"); "not returned" }
#' fn()
#'
#'
#' # rst_maybe_jump() checks that a restart exists before trying to jump:
#' fn <- function() {
#'   g()
#'   cat("will this be called?\n")
#' }
#' g <- function() {
#'   rst_maybe_jump("my_restart")
#'   cat("will this be called?\n")
#' }
#'
#' # Here no restart are on the stack:
#' fn()
#'
#' # If a restart point called `my_restart` was established on the
#' # stack before calling fn(), the control flow will jump there:
#' rst <- function() {
#'   cat("restarting...\n")
#'   "return value"
#' }
#' with_restarts(fn(), my_restart = rst)
#'
#'
#' # Restarts are particularly useful to provide alternative default
#' # values when the normal output cannot be computed:
#'
#' fn <- function(valid_input) {
#'   if (valid_input) {
#'     return("normal value")
#'   }
#'
#'   # We decide to return the empty string "" as default value. An
#'   # altenative strategy would be to signal an error. In any case,
#'   # we want to provide a way for the caller to get a different
#'   # output. For this purpose, we provide two restart functions that
#'   # returns alternative defaults:
#'   restarts <- list(
#'     rst_empty_chr = function() character(0),
#'     rst_null = function() NULL
#'   )
#'
#'   with_restarts(splice(restarts), .expr = {
#'
#'     # Signal a typed condition to let the caller know that we are
#'     # about to return an empty string as default value:
#'     cnd_signal("default_empty_string")
#'
#'     # If no jump to with_restarts, return default value:
#'     ""
#'   })
#' }
#'
#' # Normal value for valid input:
#' fn(TRUE)
#'
#' # Default value for bad input:
#' fn(FALSE)
#'
#' # Change the default value if you need an empty character vector by
#' # defining an inplace handler that jumps to the restart. It has to
#' # be inplace because exiting handlers jump to the place where they
#' # are established before being executed, and the restart is not
#' # defined anymore at that point:
#' rst_handler <- inplace(function(c) rst_jump("rst_empty_chr"))
#' with_handlers(fn(FALSE), default_empty_string = rst_handler)
#'
#' # You can use restarting() to create restarting handlers easily:
#' with_handlers(fn(FALSE), default_empty_string = restarting("rst_null"))
with_restarts <- function(.expr, ...) {
  quo <- quo(withRestarts(expr = !! enquo(.expr), !!! dots_list(...)))
  eval_tidy(quo)
}


#' Restarts utilities
#'
#' Restarts are named jumping points established by [with_restarts()].
#' `rst_list()` returns the names of all restarts currently
#' established. `rst_exists()` checks if a given restart is
#' established. `rst_jump()` stops execution of the current function
#' and jumps to a restart point. If the restart does not exist, an
#' error is thrown.  `rst_maybe_jump()` first checks that a restart
#' exists before jumping.
#'
#' @param .restart The name of a restart.
#' @param ... Arguments passed on to the restart function. These
#'   dots are evaluated with [explicit splicing][dots_list].
#' @seealso [with_restarts()], [rst_muffle()].
#' @export
rst_list <- function() {
  computeRestarts()
}
#' @rdname rst_list
#' @export
rst_exists <- function(.restart) {
  !is.null(findRestart(.restart))
}
#' @rdname rst_list
#' @export
rst_jump <- function(.restart, ...) {
  args <- c(list(r = .restart), dots_list(...))
  do.call("invokeRestart", args)
}
#' @rdname rst_list
#' @export
rst_maybe_jump <- function(.restart, ...) {
  if (rst_exists(.restart)) {
    args <- c(list(r = .restart), dots_list(...))
    do.call("invokeRestart", args)
  }
}

#' Jump to the abort restart
#'
#' The abort restart is the only restart that is established at top
#' level. It is used by R as a top-level target, most notably when an
#' error is issued (see [abort()]) that no handler is able
#' to deal with (see [with_handlers()]).
#'
#' @seealso [rst_jump()], [abort()] and [cnd_abort()].
#' @export
#' @examples
#' # The `abort` restart is a bit special in that it is always
#' # registered in a R session. You will always find it on the restart
#' # stack because it is established at top level:
#' rst_list()
#'
#' # You can use the `above` restart to jump to top level without
#' # signalling an error:
#' \dontrun{
#' fn <- function() {
#'   cat("aborting...\n")
#'   rst_abort()
#'   cat("This is never called\n")
#' }
#' {
#'   fn()
#'   cat("This is never called\n")
#' }
#' }
#'
#' # The `above` restart is the target that R uses to jump to top
#' # level when critical errors are signalled:
#' \dontrun{
#' {
#'   abort("error")
#'   cat("This is never called\n")
#' }
#' }
#'
#' # If another `abort` restart is specified, errors are signalled as
#' # usual but then control flow resumes with from the new restart:
#' \dontrun{
#' out <- NULL
#' {
#'   out <- with_restarts(abort("error"), abort = function() "restart!")
#'   cat("This is called\n")
#' }
#' cat("`out` has now become:", out, "\n")
#' }
rst_abort <- function() {
  rst_jump("abort")
}

#' Jump to a muffling restart
#'
#' Muffle restarts are established at the same location as where a
#' condition is signalled. They are useful for two non-exclusive
#' purposes: muffling signalling functions and muffling conditions. In
#' the first case, `rst_muffle()` prevents any further side effects of
#' a signalling function (a warning or message from being displayed,
#' an aborting jump to top level, etc). In the second case, the
#' muffling jump prevents a condition from being passed on to other
#' handlers. In both cases, execution resumes normally from the point
#' where the condition was signalled.
#'
#' @param c A condition to muffle.
#' @seealso The `muffle` argument of [inplace()], and the `mufflable`
#'   argument of [cnd_signal()].
#' @export
#' @examples
#' side_effect <- function() cat("side effect!\n")
#' handler <- inplace(function(c) side_effect())
#'
#' # A muffling handler is an inplace handler that jumps to a muffle
#' # restart:
#' muffling_handler <- inplace(function(c) {
#'   side_effect()
#'   rst_muffle(c)
#' })
#'
#' # You can also create a muffling handler simply by setting
#' # muffle = TRUE:
#' muffling_handler <- inplace(function(c) side_effect(), muffle = TRUE)
#'
#' # You can then muffle the signalling function:
#' fn <- function(signal, msg) {
#'   signal(msg)
#'   "normal return value"
#' }
#' with_handlers(fn(message, "some message"), message = handler)
#' with_handlers(fn(message, "some message"), message = muffling_handler)
#' with_handlers(fn(warning, "some warning"), warning = muffling_handler)
#'
#' # Note that exiting handlers are thrown to the establishing point
#' # before being executed. At that point, the restart (established
#' # within the signalling function) does not exist anymore:
#' \dontrun{
#' with_handlers(fn(warning, "some warning"),
#'   warning = exiting(function(c) rst_muffle(c)))
#' }
#'
#'
#' # Another use case for muffle restarts is to muffle conditions
#' # themselves. That is, to prevent other condition handlers from
#' # being called:
#' undesirable_handler <- inplace(function(c) cat("please don't call me\n"))
#'
#' with_handlers(foo = undesirable_handler,
#'   with_handlers(foo = muffling_handler, {
#'     cnd_signal("foo", mufflable = TRUE)
#'     "return value"
#'   }))
#'
#' # See the `mufflable` argument of cnd_signal() for more on this point
rst_muffle <- function(c) {
  UseMethod("rst_muffle")
}
#' @export
rst_muffle.default <- function(c) {
  abort("No muffle restart defined for this condition", "control")
}
#' @export
rst_muffle.simpleMessage <- function(c) {
  rst_jump("muffleMessage")
}
#' @export
rst_muffle.simpleWarning <- function(c) {
  rst_jump("muffleWarning")
}
#' @export
rst_muffle.mufflable <- function(c) {
  rst_jump("muffle")
}
