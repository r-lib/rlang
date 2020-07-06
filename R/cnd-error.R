
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
  simplify <- arg_match(simplify)
  cat_line(format(x, simplify = simplify, fields = fields))
  invisible(x)
}

is_rlang_error <- function(x) {
  inherits(x, "rlang_error")
}

#' @export
format.rlang_error <- function(x,
                               ...,
                               child = NULL,
                               simplify = c("branch", "collapse", "none"),
                               fields = FALSE) {
  # Allow overwriting default display via condition field
  simplify <- x$rlang$internal$print_simplify %||% simplify

  orig <- x
  parent <- x$parent
  style <- cli_box_chars()

  header <- rlang_error_header(x)

  if (is_rlang_error(parent)) {
    header <- header_add_tree_node(header, style, parent)
    header <-   paste_line(trace_root(), header)

    message <- with_reduced_width(
      as_rlang_error_message(conditionMessage(x))
    )
    message <- message_add_tree_prefix(message, style, parent)
  } else {
    message <- as_rlang_error_message(conditionMessage(x))
  }

  out <- paste_line(
    header,
    message
  )

  while (is_rlang_error(parent)) {
    x <- parent
    parent <- parent$parent

    header <- rlang_error_header(x)
    header <- header_add_tree_node(header, style, parent)

    message <- with_reduced_width(
      as_rlang_error_message(cnd_header(x))
    )
    message <- message_add_tree_prefix(message, style, parent)

    if (is_rlang_error(parent)) {
      header <- paste0
      message <- with_reduced_width(
        as_rlang_error_message(conditionMessage(x))
      )
      message <- message_add_tree_prefix(message, style)
    }

    out <- paste_line(
      out,
      header,
      message
    )
  }

  trace <- x$trace
  simplify <- arg_match(simplify)

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

  # Recommend printing the full backtrace if called from `last_error()`
  from_last_error <- is_true(orig$rlang$internal$from_last_error)
  if (from_last_error && simplify == "branch" && !is_null(trace)) {
    reminder <- silver("Run `rlang::last_trace()` to see the full context.")
    out <- paste_line(out, reminder)
  }

  out
}

with_reduced_width <- function(expr) {
  with_options(
    width = max(peek_option("width") - 2L, 10L),
    expr
  )
}

as_rlang_error_message <- function(message) {
  message <- strip_trailing_newline(message)

  if (nzchar(message)) {
    message
  } else {
    NULL
  }
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
