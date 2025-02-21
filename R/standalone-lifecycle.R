# ---
# repo: r-lib/rlang
# file: standalone-lifecycle.R
# last-updated: 2024-10-05
# license: https://unlicense.org
# dependencies: standalone-cli.R
# imports: rlang (>= 1.0.0)
# ---
#
# This file serves as a reference for currently unexported rlang
# lifecycle functions. These functions require rlang in your `Imports`
# DESCRIPTION field but you don't need to import rlang in your
# namespace.
#
# ## Changelog
#
# 2024-09-27
#
# - Depends on `standalone-cli.R` instead of the cli package.
#
# 2024-09-27
#
# - Removed call to `glue::glue()` in `.rlang_lifecycle_verbosity()`
#
# 2023-02-23
#
# - Updated the API and internals to match modern lifecycle tools.
#
# 2021-04-19
#
# - Removed `lifecycle()` function. You can now use the following in
#   your roxygen documentation to inline a badge:
#
#    ```
#    `r lifecycle::badge()`
#    ```
#
#   This is a build-time dependency on lifecycle so there is no need
#   to add lifecycle to Imports just to use badges. See also
#   `?usethis::use_lifecycle()` for importing or updating the badge
#   images in your package.
#
# - Soft-namespaced private objects.
#
# nocov start

#' Signal deprecation
#'
#' @description
#' These functions provide two levels of verbosity for deprecation
#' warnings.
#'
#' * `deprecate_soft()` warns only if called directly: from the global
#'   environment (so the user can change their script) or from the
#'   package currently being tested (so the package developer can fix
#'   the package).
#'
#' * `deprecate_warn()` warns unconditionally.
#'
#' * `deprecate_stop()` fails unconditionally.
#'
#' Both functions warn only once per session by default to avoid
#' overwhelming the user with repeated warnings.
#'
#' @param msg The deprecation message.
#' @param id The id of the deprecation. A warning is issued only once
#'   for each `id`. Defaults to `msg`, but you should give a unique ID
#'   when the message is built programmatically and depends on inputs.
#' @param user_env The environment in which the deprecated function
#'   was called. The verbosity depends on whether the deprecated
#'   feature was called directly, see [rlang::env_is_user_facing()] and the
#'   documentation in the lifecycle package.
#'
#' @section Controlling verbosity:
#'
#' The verbosity of retirement warnings can be controlled with global
#' options. You'll generally want to set these options locally with
#' one of these helpers:
#'
#' * `with_lifecycle_silence()` disables all soft-deprecation and
#'   deprecation warnings.
#'
#' * `with_lifecycle_warnings()` enforces warnings for both
#'   soft-deprecated and deprecated functions. The warnings are
#'   repeated rather than signalled once per session.
#'
#' * `with_lifecycle_errors()` enforces errors for both
#'   soft-deprecated and deprecated functions.
#'
#' All the `with_` helpers have `scoped_` variants that are
#' particularly useful in testthat blocks.
#'
#' @noRd
NULL

deprecate_soft <- function(msg, id = msg, user_env = rlang::caller_env(2)) {
  .rlang_lifecycle_signal_stage(msg, "deprecated")

  id <- paste(id, collapse = "\n")
  verbosity <- .rlang_lifecycle_verbosity()

  invisible(switch(
    verbosity,
    quiet = NULL,
    warning = ,
    default = if (rlang::env_is_user_facing(user_env)) {
      always <- verbosity == "warning"
      trace <- rlang::trace_back(bottom = caller_env())
      .rlang_lifecycle_deprecate_warn0(
        msg,
        id = id,
        trace = trace,
        always = always
      )
    },
    error = deprecate_stop(msg)
  ))
}

deprecate_warn <- function(
  msg,
  id = msg,
  always = FALSE,
  user_env = rlang::caller_env(2)
) {
  .rlang_lifecycle_signal_stage(msg, "deprecated")

  id <- paste(id, collapse = "\n")
  verbosity <- .rlang_lifecycle_verbosity()

  invisible(switch(
    verbosity,
    quiet = NULL,
    warning = ,
    default = {
      direct <- rlang::env_is_user_facing(user_env)
      always <- direct && (always || verbosity == "warning")

      trace <- tryCatch(
        rlang::trace_back(bottom = rlang::caller_env()),
        error = function(...) NULL
      )

      .rlang_lifecycle_deprecate_warn0(
        msg,
        id = id,
        trace = trace,
        always = always
      )
    },
    error = deprecate_stop(msg),
  ))
}

.rlang_lifecycle_deprecate_warn0 <- function(
  msg,
  id = msg,
  trace = NULL,
  always = FALSE,
  call = rlang::caller_env()
) {
  if (always) {
    freq <- "always"
  } else {
    freq <- "regularly"
  }

  rlang::warn(
    msg,
    class = "lifecycle_warning_deprecated",
    .frequency = freq,
    .frequency_id = id
  )
}

deprecate_stop <- function(msg) {
  msg <- format_error(msg)
  .rlang_lifecycle_signal_stage(msg, "deprecated")

  stop(rlang::cnd(
    c("defunctError", "error", "condition"),
    old = NULL,
    new = NULL,
    package = NULL,
    message = msg
  ))
}

.rlang_lifecycle_signal_stage <- function(msg, stage) {
  rlang::signal(msg, "lifecycle_stage", stage = stage)
}

expect_deprecated <- function(expr, regexp = NULL, ...) {
  rlang::local_options(lifecycle_verbosity = "warning")

  if (!is.null(regexp) && rlang::is_na(regexp)) {
    rlang::abort("`regexp` can't be `NA`.")
  }

  testthat::expect_warning(
    {{ expr }},
    regexp = regexp,
    class = "lifecycle_warning_deprecated",
    ...
  )
}

local_lifecycle_silence <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "quiet"
  )
}
with_lifecycle_silence <- function(expr) {
  local_lifecycle_silence()
  expr
}

local_lifecycle_warnings <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "warning"
  )
}
with_lifecycle_warnings <- function(expr) {
  local_lifecycle_warnings()
  expr
}

local_lifecycle_errors <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "error"
  )
}
with_lifecycle_errors <- function(expr) {
  local_lifecycle_errors()
  expr
}

.rlang_lifecycle_verbosity <- function() {
  opt <- getOption("lifecycle_verbosity", "default")

  if (!rlang::is_string(opt, c("quiet", "default", "warning", "error"))) {
    options(lifecycle_verbosity = "default")
    rlang::warn(paste(
      "The `lifecycle_verbosity` option must be set to one of:",
      '"quiet", "default", "warning", or "error".',
      'Resetting to "default".'
    ))
  }

  opt
}

# nocov end
