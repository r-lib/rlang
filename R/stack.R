#' Get properties of the current or caller frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' * The current frame is the execution context of the function that
#'   is currently being evaluated.
#'
#' * The caller frame is the execution context of the function that
#'   called the function currently being evaluated.
#'
#' @section Life cycle:
#' These functions are experimental.
#'
#' @param n The number of callers to go back.
#'
#' @seealso [caller_env()] and [current_env()]
#' @keywords internal
#' @export
caller_fn <- function(n = 1) {
  delayedAssign("do", sys.function(), eval.env = caller_env(n + 1))
  do
}
#' @rdname caller_fn
#' @export
current_fn <- function() {
  caller_fn()
}
utils::globalVariables("do")

caller_call <- function(n = 1L) {
  if (is_environment(n)) {
    parent <- detect_index(sys.frames(), identical, n, .right = TRUE)
  } else {
    parent <- sys.parent(n + 1L)
  }

  if (parent) {
    sys.call(parent)
  } else {
    NULL
  }
}

#' Jump to or from a frame
#'
#' @description
#' `r lifecycle::badge("questioning")`
#'
#' While [base::return()] can only return from the current local
#' frame, `return_from()` will return from any frame on the
#' current evaluation stack, between the global and the currently
#' active context.
#'
#' @param frame An execution environment of a currently running
#'   function.
#' @param value The return value.
#'
#' @keywords internal
#' @export
#' @examples
#' fn <- function() {
#'   g(current_env())
#'   "ignored"
#' }
#' g <- function(env) {
#'   h(env)
#'   "ignored"
#' }
#' h <- function(env) {
#'   return_from(env, "early return")
#'   "ignored"
#' }
#'
#' fn()
return_from <- function(frame, value = NULL) {
  eval_bare(expr(return(!!value)), frame)
}

#' Inspect a call
#'
#' This function is a wrapper around [base::match.call()]. It returns
#' its own function call.
#'
#' @param ... Arguments to display in the returned call.
#' @export
#' @examples
#' # When you call it directly, it simply returns what you typed
#' call_inspect(foo(bar), "" %>% identity())
#'
#' # Pass `call_inspect` to functionals like `lapply()` or `map()` to
#' # inspect the calls they create around the supplied function
#' lapply(1:3, call_inspect)
call_inspect <- function(...) match.call()
