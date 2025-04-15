#' Check that all dots have been used
#'
#' When `...` arguments are passed to a method, the method should match
#' and use these arguments. If this isn't the case, this often indicates
#' a programming error. Call `check_dots_used()` to fail with an error when
#' unused arguments are detected.
#'
#' @param error An optional error handler passed to [try_fetch()]. Use
#'   this e.g. to demote an error into a warning.
#' @param action `r lifecycle::badge("deprecated")`
#' @param env Environment in which to look for `...` and to set up handler.
#' @inheritParams args_error_context
#'
#' @family dots checking functions
#' @details
#' In packages, document `...` with this standard tag:
#'
#' ```
#'  @@inheritParams rlang::args_dots_used
#' ```
#'
#' `check_dots_used()` implicitly calls [on.exit()] to check that all
#' elements of `...` have been used when the function exits. If you
#' use [on.exit()] elsewhere in your function, make sure to use `add =
#' TRUE` so that you don't override the handler set up by
#' `check_dots_used()`.
#'
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
#'
#' try(f(x = 1, y = 2, 3, 4, 5))
#'
#' # Use an `error` handler to handle the error differently.
#' # For instance to demote the error to a warning:
#' fn <- function(...) {
#'   check_dots_empty(
#'     error = function(cnd) {
#'       warning(cnd)
#'     }
#'   )
#'   "out"
#' }
#' fn()
#'
#' @export
check_dots_used <- function(
  env = caller_env(),
  call = caller_env(),
  error = NULL,
  action = deprecated()
) {
  # Capture frame environment before `caller_env()` exits (#1448)
  force(call)

  handler <- function() check_dots(env, error, action, call)
  inject(base::on.exit(!!call2(handler), add = TRUE), env)
  invisible()
}

check_dots <- function(env = caller_env(), error, action, call) {
  if (.Call(ffi_ellipsis_dots_used, env)) {
    return(invisible())
  }

  proms <- ellipsis_dots(env)
  unused <- !map_lgl(proms, promise_forced)

  action_dots(
    error = error,
    action = action,
    message = "Arguments in `...` must be used.",
    note = c("i" = "Did you misspell an argument name?"),
    dots_i = unused,
    class = "rlib_error_dots_unused",
    call = call,
    env = env
  )
}

#' Check that all dots are unnamed
#'
#' In functions like `paste()`, named arguments in `...` are often a
#' sign of misspelled argument names. Call `check_dots_unnamed()` to
#' fail with an error when named arguments are detected.
#'
#' @inheritParams check_dots_used
#' @family dots checking functions
#' @param env Environment in which to look for `...`.
#' @export
#' @examples
#' f <- function(..., foofy = 8) {
#'   check_dots_unnamed()
#'   c(...)
#' }
#'
#' f(1, 2, 3, foofy = 4)
#'
#' try(f(1, 2, 3, foof = 4))
check_dots_unnamed <- function(
  env = caller_env(),
  error = NULL,
  call = caller_env(),
  action = abort
) {
  if (.Call(ffi_has_dots_unnamed, env)) {
    return()
  }

  proms <- ellipsis_dots(env)
  unnamed <- names2(proms) == ""

  if (all(unnamed)) {
    return(invisible())
  }
  named <- !unnamed

  action_dots(
    error = error,
    action = action,
    message = "Arguments in `...` must be passed by position, not name.",
    dots_i = named,
    class = "rlib_error_dots_named",
    call = call,
    env = env
  )
}

#' Check that dots are empty
#'
#' `...` can be inserted in a function signature to force users to
#' fully name the details arguments. In this case, supplying data in
#' `...` is almost always a programming error. This function checks
#' that `...` is empty and fails otherwise.
#'
#' @inheritParams check_dots_used
#' @param env Environment in which to look for `...`.
#'
#' @family dots checking functions
#' @details
#' In packages, document `...` with this standard tag:
#'
#' ```
#'  @@inheritParams rlang::args_dots_empty
#' ```
#'
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
#'
#' @export
check_dots_empty <- function(
  env = caller_env(),
  error = NULL,
  call = caller_env(),
  action = abort
) {
  dots <- ellipsis_dots(env)
  n <- length(dots)

  if (n == 0) {
    return()
  }

  if (n == 1) {
    nms <- names(dots)
    no_name <- is_null(nms) || identical(nms[[n]], "")
    if (no_name && identical(dots[[n]], missing_arg())) {
      return()
    }
  }

  if (!is_named(dots)) {
    note <- c("i" = "Did you forget to name an argument?")
  } else {
    note <- NULL
  }

  action_dots(
    error = error,
    action = action,
    message = "`...` must be empty.",
    note = note,
    dots_i = TRUE,
    class = "rlib_error_dots_nonempty",
    call = call,
    env = env
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
check_dots_empty0 <- function(..., call = caller_env()) {
  if (nargs()) {
    check_dots_empty(call = call)
  }
}

action_dots <- function(
  error,
  action,
  message,
  dots_i,
  env,
  class = NULL,
  note = NULL,
  ...
) {
  if (is_missing(action)) {
    action <- abort
  } else {
    # Silently deprecated for now
    paste_line(
      "The `action` argument of ellipsis functions is deprecated as of rlang 1.0.0.",
      "Please use the `error` argument instead."
    )
  }

  dots <- substitute(...(), env = env)[dots_i]

  names(dots) <- ifelse(
    have_name(dots),
    names2(dots),
    paste0("..", seq_along(dots))
  )

  bullet_header <- ngettext(
    length(dots),
    "Problematic argument:",
    "Problematic arguments:",
  )

  bullets <- map2_chr(names(dots), dots, function(name, expr) {
    sprintf("%s = %s", name, as_label(expr))
  })

  if (is_null(error)) {
    try_dots <- identity
  } else {
    try_dots <- function(expr) try_fetch(expr, error = error)
  }

  try_dots(action(
    c(message, "x" = bullet_header, set_names(bullets, "*"), note),
    class = c(class, "rlib_error_dots"),
    ...
  ))
}

promise_forced <- function(x) {
  .Call(ffi_ellipsis_promise_forced, x)
}
ellipsis_dots <- function(env = caller_env()) {
  .Call(ffi_ellipsis_dots, env)
}

#' Helper for consistent documentation of empty dots
#'
#' Use `@inheritParams rlang::args_dots_empty` in your package
#' to consistently document `...` that must be empty.
#'
#' @param ... These dots are for future extensions and must be empty.
#' @name args_dots_empty
#' @keywords internal
NULL

#' Helper for consistent documentation of used dots
#'
#' Use `@inheritParams rlang::args_dots_used` in your package
#' to consistently document `...` that must be used.
#'
#' @param ... Arguments passed to methods.
#' @name args_dots_used
#' @keywords internal
NULL
