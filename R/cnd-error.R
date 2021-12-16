#' Errors of class `rlang_error`
#'
#' @description
#' [abort()] and [error_cnd()] create errors of class `"rlang_error"`.
#' The differences with base errors are:
#'
#' - Implementing `conditionMessage()` methods for subclasses of
#'   `"rlang_error"` is undefined behaviour. Instead, implement the
#'   [cnd_header()] method (and possibly [cnd_body()] and
#'   [cnd_footer()]). These methods return character vectors which are
#'   assembled by rlang when needed: when
#'   [`conditionMessage.rlang_error()`][conditionMessage] is called
#'   (e.g. via [try()]), when the error is displayed through [print()]
#'   or [format()], and of course when the error is displayed to the
#'   user by [abort()].
#'
#' - [cnd_header()], [cnd_body()], and [cnd_footer()] methods can be
#'   overridden by storing closures in the `header`, `body`, and
#'   `footer` fields of the condition. This is useful to lazily
#'   generate messages based on state captured in the closure
#'   environment.
#'
#' - `r lifecycle::badge("experimental")` The `use_cli_format`
#'   condition field instructs whether to use cli (or rlang's fallback
#'   method if cli is not installed) to format the error message at
#'   print time.
#'
#'   In this case, the `message` field may be a character vector of
#'   header and bullets. These are formatted at the last moment to
#'   take the context into account (starting position on the screen
#'   and indentation).
#'
#'   See [local_use_cli()] for automatically setting this field in
#'   errors thrown with [abort()] within your package.
#'
#' @name rlang_error
NULL

#' @rdname cnd
#' @export
error_cnd <- function(class = NULL,
                      ...,
                      message = "",
                      call = NULL,
                      trace = NULL,
                      parent = NULL,
                      use_cli_format = NULL) {
  if (!is_null(trace) && !inherits(trace, "rlang_trace")) {
    abort("`trace` must be NULL or an rlang backtrace")
  }
  if (!is_null(parent) && !inherits(parent, "condition")) {
    abort("`parent` must be NULL or a condition object")
  }

  if (is_environment(call)) {
    call <- error_call(call)
  }

  fields <- error_cnd_fields(
    trace = trace,
    parent = parent,
    ...,
    use_cli_format = use_cli_format,
    call = call
  )

  .Call(
    ffi_new_condition,
    c(class, "rlang_error", "error"),
    message,
    fields
  )
}
error_cnd_fields <- function(trace,
                             parent,
                             ...,
                             use_cli_format = NULL,
                             .subclass = NULL,
                             `_env` = caller_env(),
                             `_frame` = caller_env(2)) {
  if (!is_null(.subclass)) {
    deprecate_subclass(.subclass, `_env`)
  }

  use_cli_format <- use_cli_format %||% use_cli(`_frame`)[["format"]]

  if (is_true(use_cli_format)) {
    list2(trace = trace, parent = parent, ..., use_cli_format = TRUE)
  } else {
    list2(trace = trace, parent = parent, ...)
  }
}

#' @export
print.rlang_error <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

is_rlang_error <- function(x) {
  inherits(x, "rlang_error")
}

#' @export
format.rlang_error <- function(x,
                               ...,
                               backtrace = TRUE,
                               simplify = c("branch", "collapse", "none")) {
  # Allow overwriting default display via condition field
  simplify <- x$rlang$internal$print_simplify %||% simplify
  simplify <- arg_match(simplify)

  out <- cnd_format(x, ..., backtrace = backtrace, simplify = simplify)

  # Recommend printing the full backtrace if called from `last_error()`
  from_last_error <- is_true(x$rlang$internal$from_last_error)
  if (from_last_error && simplify == "branch" && !is_null(x$trace)) {
    reminder <- silver("Run `rlang::last_trace()` to see the full context.")
    out <- paste_line(out, reminder)
  }

  out
}

#' @export
summary.rlang_error <- function(object, ...) {
  print(object, simplify = "none")
}
