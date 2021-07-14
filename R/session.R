#' Are packages installed in any of the libraries?
#'
#' @description
#' These functions check that packages are installed with minimal side
#' effects. If installed, the packages will be loaded but not
#' attached.
#'
#' - `is_installed()` doesn't interact with the user. It simply
#'   returns `TRUE` or `FALSE` depending on whether the packages are
#'   installed.
#'
#' - In interactive sessions, `check_installed()` asks the user
#'  whether to install missing packages. If the user accepts, the
#'  packages are installed with `pak::pkg_install()` if available, or
#'  [utils::install.packages()] otherwise. If the session is non
#'  interactive or if the user chooses not to install the packages,
#'  the current evaluation is aborted.
#'
#'  You can disable the prompt by setting the
#'  `rlib_restart_package_not_found` global option to `FALSE`. In that
#'  case, missing packages always cause an error.
#'
#' @param pkg The package names.
#' @param reason Optional string indicating why is `pkg` needed.
#'   Appears in error messages (if non-interactive) and user prompts
#'   (if interactive).
#' @param ... These dots must be empty.
#' @param version Minimum versions for `pkg`. If supplied, must be the
#'   same length as `pkg`. `NA` elements stand for any versions.
#' @return `is_installed()` returns `TRUE` if _all_ package names
#'   provided in `pkg` are installed, `FALSE`
#'   otherwise. `check_installed()` either doesn't return or returns
#'   `NULL`.
#'
#' @section Handling package not found errors:
#' `check_installed()` signals error conditions of class
#' `rlib_error_package_not_found`. The error includes `pkg` and
#' `version` fields. They are vectorised and may include several
#' packages.
#'
#' The error is signalled with a `rlib_restart_package_not_found`
#' restart on the stack to allow handlers to install the required
#' packages. To do so, add a [calling handler][withCallingHandlers]
#' for `rlib_error_package_not_found`, install the required packages,
#' and invoke the restart without arguments. This restarts the check
#' from scratch.
#'
#' The condition is not signalled in non-interactive sessions, in the
#' restarting case, or if the `rlib_restart_package_not_found` user
#' option is set to `FALSE`.
#'
#' @export
#' @examples
#' is_installed("utils")
#' is_installed(c("base", "ggplot5"))
#' is_installed(c("base", "ggplot5"), version = c(NA, "5.1.0"))
is_installed <- function(pkg, ..., version = NULL) {
  check_dots_empty0(...)

  # Internal mechanism for unit tests
  hook <- peek_option("rlang:::is_installed_hook")
  if (is_function(hook)) {
    return(all(hook(pkg, version)))
  }

  if (!all(map_lgl(pkg, function(x) is_true(requireNamespace(x, quietly = TRUE))))) {
    return(FALSE)
  }
  if (is_null(version)) {
    return(TRUE)
  }

  if (!is_character(version, n = length(pkg))) {
    abort("`version` must be a character vector the same length as `pkg`.")
  }

  all(map2_lgl(pkg, version, function(p, v) {
    is_na(v) || utils::packageVersion(p) >= v
  }))
}
#' @rdname is_installed
#' @export
check_installed <- function(pkg,
                            reason = NULL,
                            ...,
                            version = NULL) {
  check_dots_empty0(...)

  if (!is_character(pkg)) {
    abort("`pkg` must be a package name or a vector of package names.")
  }

  if (is_null(version)) {
    needs_install <- !map_lgl(pkg, is_installed)
  } else {
    if (!is_character(version, n = length(pkg))) {
      abort("`version` must be a character vector the same length as `pkg`.")
    }
    needs_install <- !map2_lgl(pkg, version, function(p, v) is_installed(p, version = v))
  }

  missing_pkgs <- pkg[needs_install]
  missing_vers <- version[needs_install]

  if (!length(missing_pkgs)) {
    return(invisible(NULL))
  }

  .error_call <- FALSE

  cnd <- new_error_package_not_found(
    missing_pkgs,
    missing_vers,
    reason = reason
  )

  restart <- peek_option("rlib_restart_package_not_found") %||% TRUE
  if (!is_bool(restart)) {
    abort("`rlib_restart_package_not_found` must be a logical value.")
  }

  if (!is_interactive() || !restart) {
    abort(cnd_header(cnd))
  }

  if (signal_package_not_found(cnd)) {
    # A calling handler asked for a restart. Disable restarts and try
    # again.
    return(with_options(
      "rlib_restart_package_not_found" = FALSE,
      check_installed(pkg, reason, version = version)
    ))
  }

  header <- cnd_header(cnd)

  n <- length(missing_pkgs)
  question <- pluralise(
    n,
    "Would you like to install it?",
    "Would you like to install them?"
  )

  cat(paste_line(
    paste0(info(), " ", header),
    paste0(cross(), " ", question),
    .trailing = TRUE
  ))

  if (utils::menu(c("Yes", "No")) != 1) {
    invokeRestart("abort")
  }
  if (is_installed("pak")) {
    pak::pkg_install(missing_pkgs, ask = FALSE)
  } else {
    utils::install.packages(missing_pkgs)
  }
}

new_error_package_not_found <- function(pkg,
                                        version = NULL,
                                        ...,
                                        reason = NULL,
                                        class = NULL) {
  if (!is_character(pkg)) {
    abort("`pkg` must be character vector.")
  }
  if (!length(pkg)) {
    abort("`pkg` must contain at least one package.")
  }
  if (!is_null(version) && !is_character(version, n = length(pkg))) {
    abort("`version` must be a character vector as long as `pkg`.")
  }

  error_cnd(
    class = c(class, "rlib_error_package_not_found"),
    pkg = pkg,
    version = version,
    reason = reason,
    ...
  )
}

#' @export
cnd_header.rlib_error_package_not_found <- function(cnd, ...) {
  pkg <- cnd$pkg
  version <- cnd$version
  reason <- cnd$reason
  n <- length(pkg)

  pkg_enum <- chr_quoted(cnd$pkg)

  if (!is_null(version)) {
    pkg_enum <- map2_chr(pkg_enum, version, function(p, v) {
      if (is_na(v)) {
        p
      } else {
        paste0(p, " (>= ", v, ")")
      }
    })
  }

  pkg_enum <- chr_enumerate(pkg_enum, final = "and")

  info <- pluralise(
    n,
    paste0("The package ", pkg_enum, " is required"),
    paste0("The packages ", pkg_enum, " are required")
  )

  if (is_null(reason)) {
    paste0(info, ".")
  } else {
    paste(info, reason)
  }
}

signal_package_not_found <- function(cnd) {
  withRestarts({
    signalCondition(cnd)
    FALSE
  },
  rlib_restart_package_not_found = function() {
    TRUE
  })
}
