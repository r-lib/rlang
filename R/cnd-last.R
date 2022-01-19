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
    stop("Can't show last error because no error was recorded yet", call. = FALSE)
  }

  err$rlang$internal$from_last_error <- TRUE
  err
}
#' @rdname last_error
#' @export
last_trace <- function() {
  err <- last_error()
  err$rlang$internal$print_simplify <- "none"
  err
}

peek_last_error <- function(cnd) {
  last_error_env()$.Last.error
}
poke_last_error <- function(cnd) {
  env <- last_error_env()

  env$.Last.error <- cnd
  env$.Last.error.trace <- cnd$trace

  invisible(cnd)
}
last_error_env <- function() {
  if (!is_attached("org:r-lib")) {
    exec(
      attach,
      list(
        .Last.error = NULL,
        .Last.error.trace = NULL
      ),
      pos = length(search()),
      name = "org:r-lib"
    )
  }

  search_env("org:r-lib")
}

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
#' #> Warning in `f()`: foo
#' #> Backtrace:
#' #>  1. global f()
#' #>
#' #> [[2]]
#' #> <warning/rlang_warning>
#' #> Warning in `g()`: bar
#' #> Backtrace:
#' #>  1. global f()
#' #>  2. global g()
#' #>
#' #> [[3]]
#' #> <warning/rlang_warning>
#' #> Warning in `h()`: baz
#' #> Backtrace:
#' #>  1. global f()
#' #>  2. global g()
#' #>  3. global h()
#' ```
#'
#' To get a full backtrace, use [summary()]. In this case the full
#' backtraces do not include more information:
#'
#' ```r
#' summary(last_warnings())
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
