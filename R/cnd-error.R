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
                      trace = NULL,
                      parent = NULL) {
  if (!is_null(trace) && !inherits(trace, "rlang_trace")) {
    abort("`trace` must be NULL or an rlang backtrace")
  }
  if (!is_null(parent) && !inherits(parent, "condition")) {
    abort("`parent` must be NULL or a condition object")
  }
  fields <- error_cnd_fields(trace = trace, parent = parent, ...)

  .Call(ffi_new_condition, c(class, "rlang_error", "error"), message, fields)
}
error_cnd_fields <- function(trace, parent, ..., .subclass = NULL, env = caller_env()) {
  if (!is_null(.subclass)) {
    deprecate_subclass(.subclass, env)
  }
  list2(trace = trace, parent = parent, ...)
}

#' @export
conditionMessage.rlang_error <- function(c) {
  cnd_message(c)
}

#' @export
print.rlang_error <- function(x,
                              ...,
                              simplify = c("branch", "collapse", "none"),
                              fields = FALSE) {
  simplify <- arg_match(simplify)
  cat_line(format(x, simplify = simplify, fields = fields, ...))
  invisible(x)
}

is_rlang_error <- function(x) {
  inherits(x, "rlang_error")
}

#' @export
format.rlang_error <- function(x,
                               ...,
                               backtrace = TRUE,
                               child = NULL,
                               simplify = c("branch", "collapse", "none"),
                               fields = FALSE) {
  # Allow overwriting default display via condition field
  simplify <- x$rlang$internal$print_simplify %||% simplify
  simplify <- arg_match(simplify)

  orig <- x
  parent <- x$parent
  style <- cli_box_chars()

  header <- rlang_error_header(x)
  message <- cnd_prefix_error_message(x)

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

    message <- cnd_prefix_error_message(x, parent = TRUE)
    out <- paste_line(out, message)
  }

  if (can_paste_trace(backtrace, trace)) {
    out <- paste_trace(out, trace, simplify, ...)
  }

  if (simplify != "branch" && !is_null(x$parent)) {
    parent_lines <- format.rlang_error(x$parent, ..., child = x, simplify = simplify, fields = fields)
    out <- paste_line(out, parent_lines)
  }

  # Recommend printing the full backtrace if called from `last_error()`
  from_last_error <- is_true(orig$rlang$internal$from_last_error)
  if (from_last_error && simplify == "branch" && !is_null(trace)) {
    reminder <- silver("Run `rlang::last_trace()` to see the full context.")
    out <- paste_line(out, reminder)
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

header_add_tree_node <- function(header, style, parent) {
  if (is_rlang_error(parent)) {
    s <- style$j
  } else {
    s <- style$l
  }
  paste0(s, style$h, header)
}
message_add_tree_prefix <- function(message, style, parent) {
  if (is_null(message)) {
    return(NULL)
  }

  if (is_rlang_error(parent)) {
    s <- style$v
  } else {
    s <- " "
  }
  message <- split_lines(message)
  message <- paste0(s, " ", message)
  paste_line(message)
}


#' @export
summary.rlang_error <- function(object, ...) {
  print(object, simplify = "none", fields = TRUE)
}

rlang_error_header <- function(cnd, child = NULL) {
  class <- class(cnd)[[1]]
  if (class != "error") {
    class <- paste0("error/", class)
  }

  if (is_null(child)) {
    bold(sprintf("<%s>", class))
  } else {
    bold(sprintf("<parent: %s>", class))
  }
}

on_load(s3_register("testthat::testthat_print", "rlang_error", function(x) {
  print(x, backtrace = FALSE)
}))
