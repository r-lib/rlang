# nocov start - compat-lifecycle (last updated: rlang 0.3.0.9000)

# This file serves as a reference for currently unexported rlang
# lifecycle functions. Please find the most recent version in rlang's
# repository.


#' Signal deprecation
#'
#' `signal_soft_deprecated()` warns only if option is set, the package
#'  is attached, or if called from the global environment.
#'  `warn_deprecated()` warns unconditionally. Both functions warn
#'  only once.
#'
#' @param msg The deprecation message.
#' @param id The id of the deprecation. A warning is issued only once
#'   for each `id`. Defaults to `msg`, but you should give a unique ID
#'   when the message is built programmatically and depends on inputs.
#' @param package The soft-deprecation warning is forced when this
#'   package is attached. Automatically detected from the caller
#'   environment.
#'
#' @noRd
NULL

signal_soft_deprecated <- function(msg,
                                   id = msg,
                                   package = NULL,
                                   env = caller_env(2)) {
  if (is_true(peek_option("lifecycle_disable_verbose_retirement"))) {
    return(invisible(NULL))
  }

  if (is_true(peek_option("lifecycle_force_verbose_retirement")) ||
      is_reference(env, global_env())) {
    warn_deprecated(msg, id)
    return(invisible(NULL))
  }

  if (package_attached(package, caller_env())) {
    warn_deprecated(msg, id)
    return(invisible(NULL))
  }

  signal(msg, "lifecycle_soft_deprecated")
}

package_attached <- function(package, env) {
  if (is_null(package)) {
    top <- topenv(env)
    if (!is_namespace(top)) {
      abort("`signal_soft_deprecated()` must be called from a package function")
    }
    package <- ns_env_name(top)
  } else {
    stopifnot(is_string(package))
  }

  pkg_env_name(package) %in% search()
}

warn_deprecated <- function(msg, id = msg) {
  if (is_true(peek_option("lifecycle_disable_verbose_retirement"))) {
    return(invisible(NULL))
  }
  if (env_has(deprecation_env, id)) {
    return(invisible(NULL))
  }

  env_poke(deprecation_env, id, TRUE);

  .Deprecated(msg = paste_line(
    msg,
    silver("This warning is displayed once per session.")
  ))
}
deprecation_env <- new.env(parent = emptyenv())

abort_defunct <- function(msg) {
  .Defunct(msg = msg)
}


# nocov end
