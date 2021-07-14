#' Helper for consistent documentation of empty dots
#'
#' Use `` @inheritParams rlang::`dots-empty` `` in your package
#' to consistently document an unused `...` argument.
#'
#' @param ... These dots are for future extensions and must be empty.
#' @name dots-empty
#' @keywords internal
NULL

#' Helper for consistent documentation of used dots
#'
#' Use `` @inheritParams rlang::`dots-used` `` in your package
#' to consistently document an unused `...` argument.
#'
#' @param ... Arguments passed to methods.
#' @name dots-used
#' @keywords internal
NULL

#' Check that all dots have been used
#'
#' Automatically sets exit handler to run when function terminates, checking
#' that all elements of `...` have been evaluated. If you use [on.exit()]
#' elsewhere in your function, make sure to use `add = TRUE` so that you
#' don't override the handler set up by `check_dots_used()`.
#'
#' @param action The action to take when the dots have not been used. One of
#'   [rlang::abort()], [rlang::warn()], [rlang::inform()] or [rlang::signal()].
#' @param env Environment in which to look for `...` and to set up handler.
#' @export
#' @examples
#' f <- function(...) {
#'   check_dots_used()
#'   g(...)
#' }
#'
#' g <- function(x, y, ...) {
#'   x + y
#' }
#' f(x = 1, y = 2)
#'
#' try(f(x = 1, y = 2, z = 3))
#' try(f(x = 1, y = 2, 3, 4, 5))
check_dots_used <- function(env = caller_env(),
                            error_call = caller_env(),
                            action = abort) {
  handler <- function() check_dots(env, action, error_call)
  inject(base::on.exit(!!call2(handler), add = TRUE), env)
  invisible()
}

check_dots <- function(env = caller_env(), action, error_call) {
  if (.Call(ffi_ellipsis_dots_used, env)) {
    return(invisible())
  }

  proms <- ellipsis_dots(env)
  used <- map_lgl(proms, promise_forced)

  unused <- names(proms)[!used]
  action_dots(
    action = action,
    message = paste0(length(unused), " arguments in `...` were not used."),
    dot_names = unused,
    class = "rlib_error_dots_unused",
    call = error_call
  )
}

#' Check that all dots are unnamed
#'
#' In functions like `paste()`, named arguments in `...` are often a
#' sign of misspelled argument names.
#'
#' @inheritParams check_dots_used
#' @inheritParams error_call
#' @param env Environment in which to look for `...`.
#' @export
#' @examples
#' f <- function(..., foofy = 8) {
#'   check_dots_unnamed()
#'   c(...)
#' }
#'
#' f(1, 2, 3, foofy = 4)
#' try(f(1, 2, 3, foof = 4))
check_dots_unnamed <- function(env = caller_env(),
                               error_call = caller_env(),
                               action = abort) {
  proms <- ellipsis_dots(env, auto_name = FALSE)
  if (length(proms) == 0) {
    return()
  }

  unnamed <- is.na(names(proms))
  if (all(unnamed)) {
    return(invisible())
  }

  named <- names(proms)[!unnamed]
  action_dots(
    action = action,
    message = paste0(length(named), " arguments in `...` had unexpected names."),
    dot_names = named,
    class = "rlib_error_dots_named",
    call = error_call
  )
}


#' Check that dots are empty
#'
#' Sometimes you just want to use `...` to force your users to fully name
#' the details arguments. This function fails if `...` is not empty.
#'
#' @inheritParams check_dots_used
#' @param env Environment in which to look for `...`.
#' @export
#' @examples
#' f <- function(x, ..., foofy = 8) {
#'   check_dots_empty()
#'   x + foofy
#' }
#'
#' # This fails because `foofy` can't be matched positionally
#' try(f(1, 4))
#'
#' # This fails because `foofy` can't be matched partially by name
#' try(f(1, foof = 4))
#'
#' # Thanks to `...`, it must be matched exactly
#' f(1, foofy = 4)
check_dots_empty <- function(env = caller_env(),
                             error_call = caller_env(),
                             action = abort) {
  dots <- ellipsis_dots(env)
  if (length(dots) == 0) {
    return()
  }

  action_dots(
    action = action,
    message = "`...` is not empty.",
    dot_names = names(dots),
    note = "These dots only exist to allow future extensions and should be empty.",
    class = "rlib_error_dots_nonempty",
    call = error_call
  )
}
#' Check that dots are empty (low level variant)
#'
#' `check_dots_empty0()` is a more efficient version of
#' [check_dots_empty()] with a slightly different interface. Instead
#' of inspecting the current environment for dots, it directly takes
#' `...`. It is only meant for very low level functions where a
#' couple microseconds make a difference.
#'
#' @param ... Dots which should be empty.
#' @keywords internal
#' @export
check_dots_empty0 <- function(..., error_call = caller_env()) {
  if (nargs()) {
    check_dots_empty(error_call = error_call)
  }
}

action_dots <- function(action, message, dot_names, note = NULL, class = NULL, ...) {
  message <- c(
    message,
    i = note,
    x = "We detected these problematic arguments:",
    set_names(chr_quoted(dot_names), "*"),
    i = "Did you misspecify an argument?"
  )
  action(message, class = c(class, "rlib_error_dots"), ...)
}

promise_forced <- function(x) {
  .Call(ffi_ellipsis_promise_forced, x)
}
ellipsis_dots <- function(env = caller_env(), auto_name = TRUE) {
  .Call(ffi_ellipsis_dots, env, auto_name)
}
