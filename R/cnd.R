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
#' @seealso [cnd_signal()], [try_catch()].
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
  arg_require(class)
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

#' Does a condition or its ancestors inherit from a class?
#'
#' @description
#' Like any R objects, errors captured with catchers like
#' [tryCatch()] have a [class()] which you can test with [inherits()].
#' However, with [chained errors][topic-error-chaining], the class of
#' a captured error might be different than the error that was
#' originally signalled. Use `cnd_inherits()` to detect whether an
#' error or any of its _parent_ inherits from a class.
#'
#' Whereas `inherits()` tells you whether an object is a particular
#' kind of error, `cnd_inherits()` answers the question whether an
#' object is a particular kind of error or has been caused by such an
#' error.
#'
#'
#' # Capture an error with `cnd_inherits()`
#'
#' Error catchers like [tryCatch()] and [try_catch()] can only match
#' the class of a condition, not the class of its parents. To match a
#' class across the ancestry of an error, you'll need a bit of
#' craftiness.
#'
#' Ancestry matching can't be done with `tryCatch()` at all so you'll
#' need to switch to [withCallingHandlers()]. Alternatively, you can
#' use the experimental rlang function [try_catch()] which is able to
#' perform the roles of both `tryCatch()` and `withCallingHandlers()`.
#'
#'
#' ## `withCallingHandlers()`
#'
#' Unlike `tryCatch()`, `withCallingHandlers()` does not capture an
#' error. If you don't explicitly jump with an _error_ or a _value_
#' throw, nothing happens.
#'
#' Since we don't want to throw an error, we'll throw a value using
#' [callCC()]:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' f <- function() {
#'   parent <- error_cnd("bar", message = "Bar")
#'   abort("Foo", parent = parent)
#' }
#'
#' cnd <- callCC(function(throw) {
#'   withCallingHandlers(
#'     f(),
#'     error = function(x) if (cnd_inherits(x, "bar")) throw(x)
#'   )
#' })
#'
#' class(cnd)
#' class(cnd$parent)
#' ```
#'
#'
#' ## `try_catch()`
#'
#' This pattern is easier with [try_catch()]. Like
#' `withCallingHandlers()`, it doesn't capture a matching error right
#' away. Instead, it captures it only if the handler doesn't return a
#' [zap()] value.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' cnd <- try_catch(
#'   f(),
#'   error = function(x) if (cnd_inherits(x, "bar")) x else zap()
#' )
#'
#' class(cnd)
#' class(cnd$parent)
#' ```
#'
#' @param cnd A condition to test.
#' @param class A class passed to [inherits()].
#'
#' @seealso
#' * `r link("topic_error_chaining")`
#'
#' @export
cnd_inherits <- function(cnd, class) {
  while (is_condition(cnd)) {
    if (inherits(cnd, class)) {
      return(TRUE)
    }

    cnd <- cnd$parent
  }

  FALSE
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
  message <- cnd_message_format_prefixed(x)

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

    message <- cnd_message_format_prefixed(x, parent = TRUE)
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
