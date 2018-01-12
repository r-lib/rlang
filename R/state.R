#' Change global options
#'
#' @description
#'
#' * `scoped_options()` changes options for the duration of a stack
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
#' @param ... For `scoped_options()` and `push_options()`, named
#'   values defining new option values. For `peek_options()`, strings
#'   or character vectors of option names.
#' @param .frame The environment of a stack frame which defines the
#'   scope of the temporary options. When the frame returns, the
#'   options are set back to their original values.
#' @return For `scoped_options()` and `push_options()`, the old option
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
#'   scoped_options(my_option = 100)
#'   peek_option("my_option")
#' }
#' fn()
#' peek_option("my_option")
#'
#' # The plural peek returns a named list:
#' peek_options("my_option")
#' peek_options("my_option", "digits")
scoped_options <- function(..., .frame = caller_env()) {
  options <- dots_list(...)
  stopifnot(is_named(options))

  old <- options(options)
  options_lang <- call2(base::options, !!! old)
  scoped_exit(!! options_lang, frame = .frame)

  invisible(old)
}

#' @rdname scoped_options
#' @param .expr An expression to evaluate with temporary options.
#' @export
with_options <- function(.expr, ...) {
  scoped_options(...)
  .expr
}
#' @rdname scoped_options
#' @export
push_options <- function(...) {
  options(dots_list(...))
}
#' @rdname scoped_options
#' @export
peek_options <- function(...) {
  names <- set_names(chr(...))
  map(names, getOption)
}
#' @rdname scoped_options
#' @param name An option name as string.
#' @export
peek_option <- function(name) {
  getOption(name)
}
