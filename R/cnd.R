#' Create a condition object
#'
#' @description
#' These constructors create subclassed conditions, the objects that
#' power the error, warning, and message system in R.
#'
#' * `cnd()` creates bare conditions that only inherit from
#'   `condition`.
#'
#' * Conditions created with `error_cnd()`, `warning_cnd()`, and
#'   `message_cnd()` inherit from `"error"`, `"warning"`, or `"message"`.
#'
#' * `error_cnd()` creates subclassed errors. See
#'   [`"rlang_error"`][rlang_error].
#'
#' Use [cnd_signal()] to emit the relevant signal for a particular
#' condition class.
#'
#' @param class The condition subclass.
#' @param ... <[dynamic][dyn-dots]> Named data fields stored inside
#'   the condition object.
#' @param message A default message to inform the user about the
#'   condition when it is signalled.
#' @param trace A `trace` object created by [trace_back()].
#' @param parent A parent condition object created by [abort()].
#' @seealso [cnd_signal()], [with_handlers()].
#'
#' @keywords internal
#' @export
#' @examples
#' # Create a condition inheriting only from the S3 class "foo":
#' cnd <- cnd("foo")
#'
#' # Signal the condition to potential handlers. Since this is a bare
#' # condition the signal has no effect if no handlers are set up:
#' cnd_signal(cnd)
#'
#' # When a relevant handler is set up, the signal transfers control
#' # to the handler
#' with_handlers(cnd_signal(cnd), foo = function(c) "caught!")
#' tryCatch(cnd_signal(cnd), foo = function(c) "caught!")
cnd <- function(class, ..., message = "") {
  if (missing(class)) {
    abort("Bare conditions must be subclassed")
  }
  .Call(ffi_new_condition, class, message, cnd_fields(...))
}
#' @rdname cnd
#' @export
warning_cnd <- function(class = NULL, ..., message = "") {
  .Call(ffi_new_condition, c(class, "rlang_warning", "warning"), message, cnd_fields(...))
}
#' @rdname cnd
#' @export
message_cnd <- function(class = NULL, ..., message = "") {
  .Call(ffi_new_condition, c(class, "rlang_message", "message"), message, cnd_fields(...))
}

cnd_fields <- function(..., .subclass = NULL, env = caller_env()) {
  if (!is_null(.subclass)) {
    deprecate_subclass(.subclass, env)
  }
  dots_list(...)
}

#' Is object a condition?
#' @param x An object to test.
#' @keywords internal
#' @export
is_condition <- function(x) {
  inherits(x, "condition")
}
#' @rdname is_condition
#' @export
is_error <- function(x) {
  inherits(x, "error")
}
#' @rdname is_condition
#' @export
is_warning <- function(x) {
  inherits(x, "warning")
}
#' @rdname is_condition
#' @export
is_message <- function(x) {
  inherits(x, "message")
}

#' What type is a condition?
#'
#' Use `cnd_type()` to check what type a condition is.
#'
#' @param cnd A condition object.
#' @return A string, either `"condition"`, `"message"`, `"warning"`,
#'   `"error"` or `"interrupt"`.
#'
#' @keywords internal
#' @export
#' @examples
#' cnd_type(catch_cnd(abort("Abort!")))
#' cnd_type(catch_cnd(interrupt()))
cnd_type <- function(cnd) {
  .Call(ffi_cnd_type, cnd)
}

#' @export
print.rlang_warning <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
#' @export
summary.rlang_warning <- function(object, ...) {
  print(object, ..., simplify = "none")
}
#' @export
format.rlang_warning <- function(x,
                                 ...,
                                 backtrace = TRUE,
                                 simplify = c("branch", "collapse", "none")) {
  cnd_format(x, ..., backtrace = backtrace, simplify = simplify)
}

#' @export
print.rlang_message <- print.rlang_warning
#' @export
summary.rlang_message <- summary.rlang_warning
#' @export
format.rlang_message <- format.rlang_warning

cnd_print <- function(x, ...) {
  writeLines(cnd_format(x, ...))
  invisible(x)
}
cnd_format <- function(x,
                       ...,
                       backtrace = TRUE,
                       simplify = c("branch", "collapse", "none")) {
  simplify <- arg_match(simplify)

  orig <- x
  parent <- x$parent
  style <- cli_box_chars()

  header <- cnd_type_header(x)
  message <- cnd_prefixed_message(x)

  out <- paste_line(
    header,
    message
  )

  trace <- x$trace

  while (!is_null(parent)) {
    x <- parent
    parent <- parent$parent

    chained_trace <- x$trace
    if (can_paste_trace(backtrace, chained_trace) &&
        !identical(trace, chained_trace)) {
      out <- paste_trace(out, trace, simplify, ...)
      trace <- chained_trace
    }

    message <- cnd_prefixed_message(x, parent = TRUE)
    out <- paste_line(out, message)
  }

  if (can_paste_trace(backtrace, trace)) {
    out <- paste_trace(out, trace, simplify, ...)
  }

  out
}

can_paste_trace <- function(backtrace, trace) {
  backtrace && is_trace(trace) && trace_length(trace)
}
paste_trace <- function(x, trace, simplify, ...) {
  trace_lines <- format(trace, ..., simplify = simplify)
  paste_line(x, bold("Backtrace:"), trace_lines)
}

cnd_type_header <- function(cnd) {
  type <- cnd_type(cnd)
  class <- class(cnd)[[1]]

  if (class != type) {
    class <- c(type, class)
  }

  bold(format_cls(class))
}

testthat_print_cnd <- function(x, ...) {
  print(x, backtrace = FALSE)
}
on_load({
  s3_register("testthat::testthat_print", "rlang_error", testthat_print_cnd)
  s3_register("testthat::testthat_print", "rlang_warning", testthat_print_cnd)
  s3_register("testthat::testthat_print", "rlang_message", testthat_print_cnd)
})
