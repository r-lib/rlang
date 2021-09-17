#' Display last warnings
#'
#' @description
#'
#' * `last_warnings()` and `last_messages()` return a list of all
#'   warnings and messages that occurred during the last top-level R
#'   command.
#'
#' * `last_warning()` and `last_message()` return the very last ones.
#'
#' If you call these in the console, these warnings are printed with a
#' backtrace. Use `print(last_warnings(), simplify = level)` to
#' control the verbosity of the backtrace. The `simplify` argument
#' supports one of `"branch"` (the default), `"collapse"`, and
#' `"none"` (in increasing order of verbosity).
#'
#' @export
last_warnings <- function() {
  the$last_warnings
}
#' @rdname last_warnings
#' @export
last_messages <- function() {
  the$last_messages
}
#' @rdname last_warnings
#' @export
last_warning <- function() {
  warnings <- last_warnings()
  
  if (n <- length(warnings)) {
    warnings[[n]]
  } else {
    NULL
  }
}
#' @rdname last_warnings
#' @export
last_message <- function() {
  messages <- last_messages()
  
  if (n <- length(messages)) {
    messages[[n]]
  } else {
    NULL
  }
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
