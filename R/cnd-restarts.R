#' Establish a restart point on the stack
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' Restart points are named functions that are established with
#' `with_restarts()`. Once established, you can interrupt the normal
#' execution of R code, jump to the restart, and resume execution from
#' there. Each restart is established along with a restart function
#' that is executed after the jump and that provides a return value
#' from the establishing point (i.e., a return value for
#' `with_restarts()`).
#'
#' @param .expr An expression to execute with new restarts established
#'   on the stack. This argument is passed by expression and supports
#'   [unquoting][quasiquotation]. It is evaluated in a context where
#'   restarts are established.
#' @param ... Named restart functions. The name is taken as the
#'   restart name and the function is executed after the jump. These
#'   dots support [tidy dots][tidy-dots] features.
#' @seealso [return_from()] and [return_to()] for a more flexible way
#'   of performing a non-local jump to an arbitrary call frame.
#'
#' @details
#'
#' Restarts are not the only way of jumping to a previous call frame
#' (see [return_from()] or [return_to()]). However, they have the
#' advantage of being callable by name once established.
#'
#' @section Life cycle:
#'
#' All the restart functions are in the questioning stage. It is not
#' clear yet whether we want to recommend restarts as a style of
#' programming in R.
#'
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
#' fn <- function() g(current_env())
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
#' # defining a calling handler that jumps to the restart. It has to
#' # be calling because exiting handlers jump to the place where they
#' # are established before being executed, and the restart is not
#' # defined anymore at that point:
#' rst_handler <- calling(function(c) rst_jump("rst_empty_chr"))
#' with_handlers(fn(FALSE), default_empty_string = rst_handler)
#'
#' # You can use restarting() to create restarting handlers easily:
#' with_handlers(fn(FALSE), default_empty_string = restarting("rst_null"))
with_restarts <- function(.expr, ...) {
  do.call("withRestarts", list2(expr = quote(.expr), ...))
}


#' Restarts utilities
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
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
#'   dots support [tidy dots][tidy-dots] features.
#' @seealso [with_restarts()]
#'
#' @section Life cycle:
#'
#' All the restart functions are in the questioning stage. It is not
#' clear yet whether we want to recommend restarts as a style of
#' programming in R.
#'
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
    args <- list2(r = .restart, ...)
    do.call("invokeRestart", args)
  }
}

#' Jump to the abort restart
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' The abort restart is the only restart that is established at top
#' level. It is used by R as a top-level target, most notably when an
#' error is issued (see [abort()]) that no handler is able
#' to deal with (see [with_handlers()]).
#'
#' @section Life cycle:
#'
#' All the restart functions are in the questioning stage. It is not
#' clear yet whether we want to recommend restarts as a style of
#' programming in R.
#'
#' @seealso [rst_jump()], [abort()]
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
