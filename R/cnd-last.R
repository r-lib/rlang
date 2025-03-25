#' Last `abort()` error
#'
#' @description
#' * `last_error()` returns the last error entraced by [abort()] or
#'   [global_entrace()]. The error is printed with a backtrace in
#'   simplified form.
#'
#' * `last_trace()` is a shortcut to return the backtrace stored in
#'   the last error. This backtrace is printed in full form.
#'
#' @seealso
#' * [`rlang_backtrace_on_error`] to control what is displayed when an
#'   error is thrown.
#'
#' * [global_entrace()] to enable `last_error()` logging for all errors.
#'
#' * [last_warnings()] and [last_messages()].
#'
#' @export
last_error <- function() {
  err <- peek_last_error()

  if (is_null(err)) {
    local_options(rlang_backtrace_on_error = "none")
    stop(
      "Can't show last error because no error was recorded yet",
      call. = FALSE
    )
  }

  err$rlang$internal$from_last_error <- TRUE

  err
}
#' @rdname last_error
#' @param drop Whether to drop technical calls. These are hidden from
#'   users by default, set `drop` to `FALSE` to see the full backtrace.
#' @export
last_trace <- function(drop = NULL) {
  err <- last_error()

  # Drop by default with new tree display, don't drop with legacy
  # behaviour
  drop <- drop %||% TRUE

  err$rlang$internal$trace_simplify <- "none"
  err$rlang$internal$trace_drop <- drop

  err
}

peek_last_error <- function(cnd) {
  the$last_error
}
poke_last_error <- function(cnd) {
  the$last_error <- cnd
}
on_load(
  the$last_error <- NULL
)

#' Display last messages and warnings
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
#' @seealso [last_error()]
#'
#' @section Examples:
#'
#' Enable backtrace capture with `global_entrace()`:
#'
#' ```r
#' global_entrace()
#' ```
#'
#' Signal some warnings in nested functions. The warnings inform about
#' which function emitted a warning but they don't provide information
#' about the call stack:
#'
#' ```r
#' f <- function() { warning("foo"); g() }
#' g <- function() { warning("bar", immediate. = TRUE); h() }
#' h <- function() warning("baz")
#'
#' f()
#' #> Warning in g() : bar
#' #> Warning messages:
#' #> 1: In f() : foo
#' #> 2: In h() : baz
#' ```
#'
#' Call `last_warnings()` to see backtraces for each of these warnings:
#'
#' ```r
#' last_warnings()
#' #> [[1]]
#' #> <warning/rlang_warning>
#' #> Warning in `f()`:
#' #> foo
#' #> Backtrace:
#' #>     x
#' #>  1. \-global f()
#' #>
#' #> [[2]]
#' #> <warning/rlang_warning>
#' #> Warning in `g()`:
#' #> bar
#' #> Backtrace:
#' #>     x
#' #>  1. \-global f()
#' #>  2.   \-global g()
#' #>
#' #> [[3]]
#' #> <warning/rlang_warning>
#' #> Warning in `h()`:
#' #> baz
#' #> Backtrace:
#' #>     x
#' #>  1. \-global f()
#' #>  2.   \-global g()
#' #>  3.     \-global h()
#' ```
#'
#' This works similarly with messages:
#'
#' ```r
#' f <- function() { inform("Hey!"); g() }
#' g <- function() { inform("Hi!"); h() }
#' h <- function() inform("Hello!")
#'
#' f()
#' #> Hey!
#' #> Hi!
#' #> Hello!
#'
#' rlang::last_messages()
#' #> [[1]]
#' #> <message/rlang_message>
#' #> Message:
#' #> Hey!
#' #> ---
#' #> Backtrace:
#' #>     x
#' #>  1. \-global f()
#' #>
#' #> [[2]]
#' #> <message/rlang_message>
#' #> Message:
#' #> Hi!
#' #> ---
#' #> Backtrace:
#' #>     x
#' #>  1. \-global f()
#' #>  2.   \-global g()
#' #>
#' #> [[3]]
#' #> <message/rlang_message>
#' #> Message:
#' #> Hello!
#' #> ---
#' #> Backtrace:
#' #>     x
#' #>  1. \-global f()
#' #>  2.   \-global g()
#' #>  3.     \-global h()
#' ```
#'
#' @export
last_warnings <- function(n = NULL) {
  out <- new_list_of_conditions(the$last_warnings)
  utils::tail(out, n = n %||% length(out))
}
#' @rdname last_warnings
#' @export
last_messages <- function(n = NULL) {
  out <- new_list_of_conditions(the$last_messages)
  utils::tail(out, n = n %||% length(out))
}

on_load({
  the$n_conditions <- 0L
  the$last_top_frame <- NULL
  the$last_warnings <- list()
  the$last_messages <- list()
})

# We collect warnings/messages in a list as long as the first frame on
# the call stack has the same address. If there is a new address, it
# must be that a new top-level R command is running and we start a new
# list. This heuristic is technically not 100% correct. We might be
# very unlucky: If (a) a GC occur between two commands (b) the first
# frame on the stack reuses the same address as the last first frame
# on the stack, then we'll wrongly keep collecting warnings instead of
# starting anew.
push_warning <- function(cnd) {
  push_condition(cnd, "last_warnings")
}
push_message <- function(cnd) {
  push_condition(cnd, "last_messages")
}

push_condition <- function(cnd, last) {
  top <- obj_address(cmd_frame())

  if (has_new_cmd_frame(top)) {
    the$last_top_frame <- top
    the[[last]] <- list(cnd)
    the$n_conditions <- 1L
  } else {
    the[[last]] <- c(the[[last]], list(cnd))

    # Count the number of pushed conditions to avoid entracing too many
    # times (#1473)
    the$n_conditions <- the$n_conditions + 1L
  }
}

cmd_frame <- function() {
  if (knitr_in_progress()) {
    ns <- detect(sys.frames(), function(f) env_top_name(f) == "knitr")
    ns %||% sys.frame(1)
  } else {
    getOption("rlang:::cnd_frame", sys.frame(1))
  }
}

env_top_name <- function(x) {
  x <- topenv(x)
  if (is_namespace(x)) {
    ns_env_name(x)
  } else {
    ""
  }
}

has_new_cmd_frame <- function(top = obj_address(cmd_frame())) {
  !identical(the$last_top_frame, top)
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
