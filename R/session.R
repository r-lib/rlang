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
#' @param pkg The package names. Can include version requirements,
#'   e.g. `"pkg (>= 1.0.0)"`.
#' @param reason Optional string indicating why is `pkg` needed.
#'   Appears in error messages (if non-interactive) and user prompts
#'   (if interactive).
#' @param ... These dots must be empty.
#' @param version Minimum versions for `pkg`. If supplied, must be the
#'   same length as `pkg`. `NA` elements stand for any versions.
#' @param compare A character vector of comparison operators to use
#'   for `version`. If supplied, must be the same length as
#'   `version`. If `NULL`, `>=` is used as default for all
#'   elements. `NA` elements in `compare` are also set to `>=` by
#'   default.
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
is_installed <- function(pkg, ..., version = NULL, compare = NULL) {
  check_dots_empty0(...)

  info <- pkg_version_info(pkg, version = version, compare = compare)
  all(detect_installed(info))
}
detect_installed <- function(info) {
  # Internal mechanism for unit tests
  hook <- peek_option("rlang:::is_installed_hook")
  if (is_function(hook)) {
    return(all(hook(info$pkg, info$ver, info$cmp)))
  }

  flatten_lgl(pmap(info, function(pkg, cmp, ver) {
    if (is_string(pkg, "base")) {
      # Special-case the base package because it is not locked on
      # older R versions
      is_fully_loaded <- TRUE
    } else {
      # Check for sealed namespaces to protect against `is_installed()`
      # being called from user hooks of `pkg` (#1378)
      is_fully_loaded <-
        requireNamespace(pkg, quietly = TRUE) &&
        env_is_locked(ns_env(pkg))
    }

    if (is_fully_loaded) {
      is_na(ver) || exec(cmp, utils::packageVersion(pkg), ver)
    } else {
      FALSE
    }
  }))
}

pkg_version_info <- function(pkg,
                             version = NULL,
                             compare = NULL,
                             call = caller_env()) {
  check_pkg_version(pkg, version, compare, call = call)

  matches <- grepl(version_regex, pkg)
  pkg_info <- as_version_info(pkg[matches], call = call)

  info <- data_frame(pkg = pkg, cmp = na_chr, ver = na_chr)
  info <- vec_assign(info, matches, pkg_info)

  if (!is_null(version)) {
    has_version <- !detect_na(version)
    is_redundant <- has_version & matches
    if (any(is_redundant)) {
      elts <- encodeString(pkg[is_redundant], quote = '"')
      msg <- c(
        sprintf(
          "Can't supply version in both %s and %s.",
          format_arg("pkg"),
          format_arg("version")
        ),
        "x" = "Redundant versions:",
        set_names(elts, "*")
      )
      abort(msg, call = call)
    }

    compare <- compare %||% ">="
    compare <- compare %|% ">="

    info$ver[has_version] <- version[has_version]
    info$cmp[has_version] <- compare
  }

  if (!all(detect_na(info$cmp) | info$cmp %in% c(">", ">=", "<", "<=", "=="))) {
    msg <- sprintf(
      '%s must be one of ">", ">=", "==" ,"<", or "<=".',
      format_arg("compare")
    )
    abort(msg, call = call)
  }

  info
}

version_regex <- "(.*) \\((.*)\\)$"

as_version_info <- function(pkg, call = caller_env()) {
  if (!length(pkg)) {
    return(data_frame(pkg = chr(), cmp = chr(), ver = chr()))
  }
  ver <- sub(version_regex, "\\2", pkg)
  ver <- strsplit(ver, " ")

  ok <- map_lgl(ver, is_character2, n = 2, missing = FALSE, empty = FALSE)

  if (!all(ok)) {
    abort(
      c(
        sprintf("Can't parse version in %s.", format_arg("pkg")),
        "x" = "Problematic versions:",
        set_names(pkg[!ok], "*"),
        "i" = "Example of expected version format: `rlang (>= 1.0.0)`."
      ),
      call = call
    )
  }

  info <- set_names(transpose(ver), c("cmp", "ver"))
  info <- map(info, flatten_chr)

  pkg <- sub(version_regex, "\\1", pkg)
  info <- c(list(pkg = pkg), info)

  new_data_frame(info, .class = "tbl")
}

#' @rdname is_installed
#' @param action An optional function taking `pkg` and `...`
#'   arguments. It is called by `check_installed()` when the user
#'   chooses to update outdated packages. The function is passed the
#'   missing and outdated packages as a character vector of names.
#' @inheritParams args_error_context
#' @export
check_installed <- function(pkg,
                            reason = NULL,
                            ...,
                            version = NULL,
                            compare = NULL,
                            action = NULL,
                            call = caller_env()) {
  check_dots_empty0(...)
  check_action(action)

  info <- pkg_version_info(pkg, version = version, compare = compare)
  needs_install <- !detect_installed(info)

  pkg <- info$pkg
  version <- info$ver
  compare <- info$cmp

  missing_pkgs <- pkg[needs_install]
  missing_vers <- version[needs_install]
  missing_cmps <- compare[needs_install]

  if (!length(missing_pkgs)) {
    return(invisible(NULL))
  }

  cnd <- new_error_package_not_found(
    missing_pkgs,
    missing_vers,
    missing_cmps,
    reason = reason,
    call = call
  )

  restart <- peek_option("rlib_restart_package_not_found") %||% TRUE
  if (!is_bool(restart)) {
    abort("`rlib_restart_package_not_found` must be a logical value.")
  }

  if (!is_interactive() || !restart || any(missing_cmps %in% c("<", "<="))) {
    stop(cnd)
  }

  if (signal_package_not_found(cnd)) {
    # A calling handler asked for a restart. Disable restarts and try
    # again.
    return(with_options(
      "rlib_restart_package_not_found" = FALSE,
      check_installed(pkg, reason, version = version, call = call)
    ))
  }

  header <- cnd_header(cnd)

  n <- length(missing_pkgs)
  question <- pluralise(
    n,
    "Would you like to install it?",
    "Would you like to install them?"
  )
  question <- paste_line(
    paste0(ansi_info(), " ", header),
    paste0(ansi_cross(), " ", question),
    .trailing = TRUE
  )

  if (is_true(peek_option("rlang:::check_installed_test_hook"))) {
    return(question)
  }

  cat(question)

  if (utils::menu(c("Yes", "No")) != 1) {
    # Pass condition in case caller sets up an `abort` restart
    invokeRestart("abort", cnd)
  }

  if (!is_null(action)) {
    action(missing_pkgs)
  } else if (is_installed("pak")) {
    pkg_install <- env_get(ns_env("pak"), "pkg_install")
    pkg_install(missing_pkgs, ask = FALSE)
  } else {
    utils::install.packages(missing_pkgs)
  }
}

check_pkg_version <- function(pkg,
                              version,
                              compare,
                              call = caller_env()) {
  if (!is_character2(pkg, missing = FALSE, empty = FALSE)) {
    abort(
      sprintf(
        "%s must be a package name or a vector of package names.",
        format_arg("pkg")
      ),
      call = call
    )
  }

  if (!is_null(version) && !is_character2(version, n = length(pkg), empty = FALSE)) {
    abort(
      sprintf(
        "%s must be `NULL` or a vector of versions the same length as %s.",
        format_arg("version"),
        format_arg("pkg")
      ),
      call = call
    )
  }

  if (!is_null(compare)) {
    if (is_null(version) || any((!detect_na(compare)) & detect_na(version))) {
      msg <- sprintf(
        "%s must be supplied when %s is supplied.",
        format_arg("version"),
        format_arg("compare")
      )
      abort(msg, call = call)
    }
  }
}

check_action <- function(action, call = caller_env()) {
  # Take `pkg`, `version`, and `compare`?
  if (!is_null(action)) {
    check_closure(action, what = "`NULL` or a function", call = call)

    if (!"..." %in% names(formals(action))) {
      msg <- sprintf(
        "%s must take a %s argument.",
        format_arg("action"),
        format_arg("...")
      )
      abort(msg, call = call)
    }
  }
}

new_error_package_not_found <- function(pkg,
                                        version = NULL,
                                        compare = NULL,
                                        ...,
                                        reason = NULL,
                                        class = NULL) {
  error_cnd(
    class = c(class, "rlib_error_package_not_found"),
    pkg = pkg,
    version = version,
    compare = compare,
    reason = reason,
    ...
  )
}

#' @export
cnd_header.rlib_error_package_not_found <- function(cnd, ...) {
  pkg <- cnd$pkg
  version <- cnd$version
  compare <- cnd$compare
  reason <- cnd$reason
  n <- length(pkg)

  pkg_enum <- chr_quoted(cnd$pkg)

  if (!is_null(version)) {
    pkg_enum <- flatten_chr(pmap(list(pkg_enum, compare, version), function(p, o, v) {
      if (is_na(v)) {
        p
      } else {
        sprintf("%s (%s %s)", p, o, v)
      }
    }))
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
  class(cnd) <- vec_remove(class(cnd), "error")

  withRestarts({
    signalCondition(cnd)
    FALSE
  },
  rlib_restart_package_not_found = function() {
    TRUE
  })
}
