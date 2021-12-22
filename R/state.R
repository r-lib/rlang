#' Change global options
#'
#' @description
#'
#' * `local_options()` changes options for the duration of a stack
#'   frame (by default the current one). Options are set back to their
#'   old values when the frame returns.
#'
#' * `with_options()` changes options while an expression is
#'   evaluated. Options are restored when the expression returns.
#'
#' * `push_options()` adds or changes options permanently.
#'
#' * `peek_option()` and `peek_options()` return option values. The
#'   former returns the option directly while the latter returns a
#'   list.
#'
#'
#' @section Life cycle:
#'
#' These functions are experimental.
#'
#' @param ... For `local_options()` and `push_options()`, named
#'   values defining new option values. For `peek_options()`, strings
#'   or character vectors of option names.
#' @param .frame The environment of a stack frame which defines the
#'   scope of the temporary options. When the frame returns, the
#'   options are set back to their original values.
#' @return For `local_options()` and `push_options()`, the old option
#'   values. `peek_option()` returns the current value of an option
#'   while the plural `peek_options()` returns a list of current
#'   option values.
#'
#' @keywords experimental
#' @export
#' @examples
#' # Store and retrieve a global option:
#' push_options(my_option = 10)
#' peek_option("my_option")
#'
#' # Change the option temporarily:
#' with_options(my_option = 100, peek_option("my_option"))
#' peek_option("my_option")
#'
#' # The scoped variant is useful within functions:
#' fn <- function() {
#'   local_options(my_option = 100)
#'   peek_option("my_option")
#' }
#' fn()
#' peek_option("my_option")
#'
#' # The plural peek returns a named list:
#' peek_options("my_option")
#' peek_options("my_option", "digits")
local_options <- function(..., .frame = caller_env()) {
  options <- list2(...)
  stopifnot(is_named(options))

  old <- options(options)
  defer(options(old), envir = .frame)

  invisible(old)
}

#' @rdname local_options
#' @param .expr An expression to evaluate with temporary options.
#' @export
with_options <- function(.expr, ...) {
  local_options(...)
  .expr
}
#' @rdname local_options
#' @export
push_options <- function(...) {
  options(list2(...))
}
#' @rdname local_options
#' @export
peek_options <- function(...) {
  names <- set_names(chr(...))
  map(names, getOption)
}
#' @rdname local_options
#' @param name An option name as string.
#' @export
peek_option <- function(name) {
  getOption(name)
}

#' Is R running interactively?
#'
#' @description
#'
#' Like [base::interactive()], `is_interactive()` returns `TRUE` when
#' the function runs interactively and `FALSE` when it runs in batch
#' mode. It also checks, in this order:
#'
#' * The `rlang_interactive` global option. If set to a single `TRUE`
#'   or `FALSE`, `is_interactive()` returns that value immediately. This
#'   escape hatch is useful in unit tests or to manually turn on
#'   interactive features in RMarkdown outputs.
#'
#' * Whether knitr or testthat is in progress, in which case
#'   `is_interactive()` returns `FALSE`.
#'
#' `with_interactive()` and `local_interactive()` set the global
#' option conveniently.
#'
#' @export
is_interactive <- function() {
  opt <- peek_option("rlang_interactive")
  if (!is_null(opt)) {
    if (!is_bool(opt)) {
      options(rlang_interactive = NULL)
      check_bool(opt, arg = "rlang_interactive")
    }
    return(opt)
  }

  if (is_true(peek_option("knitr.in.progress"))) {
    return(FALSE)
  }
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(FALSE)
  }

  interactive()
}
#' @rdname is_interactive
#' @param frame The environment of a running function which defines
#'   the scope of the temporary options. When the function returns,
#'   the options are reset to their original values.
#' @param value A single `TRUE` or `FALSE`. This overrides the return
#'   value of `is_interactive()`.
#' @export
local_interactive <- function(value = TRUE, frame = caller_env()) {
  local_options(rlang_interactive = value, .frame = frame)
}
#' @rdname is_interactive
#' @param expr An expression to evaluate with interactivity set to
#'   `value`.
#' @export
with_interactive <- function(expr, value = TRUE) {
  local_interactive(value)
  expr
}

report_in_progress <- function() {
  if (is_true(peek_option("knitr.in.progress"))) {
    return(TRUE)
  }

  if (is_true(peek_option("rstudio.notebook.executing"))) {
    return(TRUE)
  }

  FALSE
}

peek_srcref <- function() {
  .Call(ffi_peek_srcref)
}
