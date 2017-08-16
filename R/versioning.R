#' Signal a deprecated or obsolete function
#'
#' @description
#'
#' These functions signal a function as deprecated or obsolete.
#'
#' * A deprecated function does not issue a warning unless the
#'   `rlang_verbose_deprecation` option is set to `TRUE`. This is
#'   often the first stage towards obsolescence. It is recommended to
#'   stop using deprecated functions and switch to suggested
#'   replacements.
#'
#' * An obsolete function always issues a warning when called. Users
#'   should stop using that function as it might get removed in the
#'   future.
#'
#' @param name The name of the depreciated function as a string.
#' @param replacement The name of the suggested replacement as a
#'   string.
#' @param start The package version where the function was
#'   depreciated.
#' @export
#' @examples
#' # A deprecated function is signalled silently:
#' foo <- function() signal_deprecated("foo", "bar", "0.2.0")
#' foo()
#'
#' # If the `rlang_verbose_deprecation` option is TRUE, a warning is
#' # issued. If run, the following would throw a warning:
#' #> options(rlang_verbose_deprecation = TRUE)
#' #> foo()
signal_deprecated <- function(name, replacement, start = NULL) {
  if (is_true(peek_option("rlang_verbose_deprecation"))) {
    signal <- cnd_warn
  } else {
    signal <- cnd_signal
  }

  msg <- deprecation_msg("deprecated", name, replacement, start)

  signal("deprecated",
    replacement = replacement,
    start = start,
    .msg = msg
  )
}
#' @rdname signal_deprecated
#' @export
warn_obsolete <- function(name, replacement, start = NULL) {
  msg <- deprecation_msg("obsolete", name, replacement, start)

  cnd_warn("obsolete",
    replacement = replacement,
    start = start,
    .msg = msg
  )
}

deprecation_msg <- function(type, name, replacement, start = NULL) {
  stopifnot(
    is_string(name),
    is_string(replacement)
  )

  msg <- sprintf("`%s` is %s", name, type)
  if (!is_null(start)) {
    msg <- sprintf("%s as of version %s", msg, start)
  }

  paste0(msg, sprintf(", please use `%s` instead", replacement))
}

ver_check <- function(ver, n_components = 3, n_digits = 2, minor = FALSE) {
  stopifnot(
    is_version(ver),
    is_integerish(n_components),
    is_integerish(n_digits)
  )

  components <- ver_components(ver)

  if (!is_null(n_components) && length(components) != n_components) {
    msg <- "version must have %s components, not %s"
    msg <- sprintf(msg, n_components, length(components))
    abort(msg)
  }

  if (!is_null(n_digits)) {
    large <- log10(components) >= n_digits
    if (any(large)) {
      msg <- "version can't have components with more than %s digits"
      msg <- sprintf(msg, n_digits)
      abort(msg)
    }
  }

  if (!minor && components[[length(components)]] != 0) {
    abort("version can't be a minor update")
  }

  invisible(TRUE)
}

is_version <- function(x) {
  inherits(x, "package_version")
}
as_version <- function(x) {
  stopifnot(is_string(x))
  as.package_version(x)
}
new_version <- function(x) {
  stopifnot(is_integerish(x))
  as_version(paste(x, collapse = "."))
}
ver_components <- function(ver) {
  flatten_int(ver)
}
