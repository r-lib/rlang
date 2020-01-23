
#' @rdname cnd
#' @export
error_cnd <- function(.subclass = NULL,
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
  fields <- dots_list(trace = trace, parent = parent, ...)
  .Call(rlang_new_condition, c(.subclass, "rlang_error", "error"), message, fields)
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
  cat_line(format(x, simplify = simplify, fields = fields))
  invisible(x)
}

#' @export
format.rlang_error <- function(x,
                               ...,
                               child = NULL,
                               simplify = c("branch", "collapse", "none"),
                               fields = FALSE) {
  # Allow overwriting default display via condition field
  simplify <- x$rlang$internal$print_simplify %||% simplify

  header <- rlang_error_header(x, child)

  message <- strip_trailing_newline(conditionMessage(x))
  if (!nzchar(message)) {
    message <- NULL
  }

  out <- paste_line(
    header,
    message
  )

  trace <- x$trace
  simplify <- arg_match(simplify, c("collapse", "branch", "none"))

  if (!is_null(trace) && trace_length(trace)) {
    out <- paste_line(out, bold("Backtrace:"))

    if (!is_null(child)) {
      trace <- trace_trim_common(trace, child$trace)
    }

    trace_lines <- format(trace, ..., simplify = simplify)
    out <- paste_line(out, trace_lines)
  }

  if (simplify != "branch" && !is_null(x$parent)) {
    parent_lines <- format.rlang_error(x$parent, ..., child = x, simplify = simplify, fields = fields)
    out <- paste_line(out, parent_lines)
  }

  # Recommend printing the full backtrace. Only do it after having
  # printed all parent errors first.
  from_last_error <-
    is_true(x$rlang$internal$from_last_error) &&
    identical(x, last_error())

  if (from_last_error && simplify == "branch" && is_null(child) && !is_null(trace)) {
    reminder <- silver("Run `rlang::last_trace()` to see the full context.")
    out <- paste_line(out, reminder)
  }

  out
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
