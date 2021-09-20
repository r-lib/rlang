#' Display last warnings
#'
#' @description
#'
#' `last_warnings()` and `last_messages()` return a list of all
#' warnings and messages that occurred during the last R command.
#'
#' [global_entrace()] must be active in order to log the messages and
#' warnings.
#'
#' By default the warnings and messages are printed with a simplified
#' backtrace, like [last_error()]. Use `summary()` to print the
#' conditions with a full backtrace.
#'
#' @param n How many warnings or messages to display. Defaults to all.
#'
#' @examples
#' if (FALSE) {
#'   global_entrace()
#'   f <- function() { warning("foo"); g() }
#'   g <- function() { warning("bar", immediate. = TRUE); h() }
#'   h <- function() warning("baz")
#'
#'   f()
#'
#'   # Simplified backtraces
#'   last_warnings()
#'
#'   # Full backtraces
#'   summary(last_warnings())
#' }
#' @export
last_warnings <- function(n = NULL) {
  out <- new_list_of_conditions(the$last_warnings)
  tail(out, n = n %||% length(out))
}
#' @rdname last_warnings
#' @export
last_messages <- function(n = NULL) {
  out <- new_list_of_conditions(the$last_messages)
  tail(out, n = n %||% length(out))
}

on_load({
  the$last_top_frame <- NULL
  the$last_warnings <- list()
  the$last_messages <- list()
})

push_warning <- function(cnd) {
  top <- obj_address(sys.frame(1))

  if (identical(the$last_top_frame, top)) {
    the$last_warnings <- c(the$last_warnings, list(cnd))
  } else {
    the$last_top_frame <- top
    the$last_warnings <- list(cnd)
  }
}
push_message <- function(cnd) {
  top <- obj_address(sys.frame(1))

  if (identical(the$last_top_frame, top)) {
    the$last_messages <- c(the$last_messages, list(cnd))
  } else {
    the$last_top_frame <- top
    the$last_messages <- list(cnd)
  }
}

# Transform foreign warnings to rlang warnings or messages. Preserve
# existing backtraces.
as_rlang_warning <- function(cnd, trace = NULL) {
  cnd$trace <- cnd$trace %||% trace

  if (!inherits(cnd, "rlang_warning")) {
    cnd <- warning_cnd(
      message = conditionMessage(cnd),
      call = conditionCall(cnd),
      trace = cnd$trace
    )
  }

  cnd
}
as_rlang_message <- function(cnd, trace = NULL) {
  cnd$trace <- cnd$trace %||% trace

  if (!inherits(cnd, "rlang_message")) {
    cnd <- message_cnd(
      message = conditionMessage(cnd),
      call = conditionCall(cnd),
      trace = cnd$trace
    )
  }

  cnd
}

new_list_of_conditions <- function(x) {
  stopifnot(every(x, is_condition))
  structure(x, class = c("rlang:::list_of_conditions", "list"))
}
#' @export
`[.rlang:::list_of_conditions` <- function(x, i) {
  new_list_of_conditions(NextMethod())
}

#' @export
`print.rlang:::list_of_conditions` <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}
#' @export
`summary.rlang:::list_of_conditions` <- function(object, ...) {
  print(unclass(object), simplify = "none", ...)
}
