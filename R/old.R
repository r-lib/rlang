#' Deprecated condition constructors
#'
#' Deprecated in 0.1.2. See [cnd()].
#'
#' @inheritParams cnd
#' @name deprecated-cnd
#' @export
new_cnd <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`new_cnd()` has been renamed to `cnd()` for consistency",
    call. = FALSE)
  cnd(.type = .type, ..., .msg = .msg)
}
#' @rdname deprecated-cnd
#' @export
cnd_error <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`cnd_error()` has been renamed to `error_cnd()` for consistency",
    call. = FALSE)
  error_cnd(.type = .type, ..., .msg = .msg)
}
#' @rdname deprecated-cnd
#' @export
cnd_warning <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`cnd_warning()` has been renamed to `warning_cnd()` for consistency",
    call. = FALSE)
  warning_cnd(.type = .type, ..., .msg = .msg)
}
#' @rdname deprecated-cnd
#' @export
cnd_message <- function(.type = NULL, ..., .msg = NULL) {
  # Deprecated in 0.1.2
  warning("`cnd_message()` has been renamed to `message_cnd()` for consistency",
    call. = FALSE)
  message_cnd(.type = .type, ..., .msg = .msg)
}
