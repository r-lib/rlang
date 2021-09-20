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
#' By default the warnings and messages are printed with a simplified
#' backtrace, like [last_error()]. Use `summary()` to print the
#' conditions with a full backtrace.
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
last_warnings <- function() {
  new_list_of_conditions(the$last_warnings)
}
#' @rdname last_warnings
#' @export
last_messages <- function() {
  new_list_of_conditions(the$last_messages)
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
}
#' @export
`summary.rlang:::list_of_conditions` <- function(object, ...) {
  print(unclass(object), simplify = "none", ...)
}
