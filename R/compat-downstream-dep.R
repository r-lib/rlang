# nocov start - compat-downstream.R
# Latest version: https://github.com/r-lib/rlang/blob/master/R/compat-downstream-deps.R

# No dependencies but uses rlang and pak if available. In interactive
# sessions the user is prompted to update outdated packages. If they
# choose no, they are informed about the global option
# `rlib_downstream_check` to turn off these prompts. In non
# interactive sessions a warning is issued. This happens when the
# outdated dep is being loaded.

# Changelog:
#
# 2020-05-07:
#
# * In interactive sessions, user is now prompted to update outdated
#   packages.
#
# * Added global option `rlib_downstream_check` to turn off prompts or
#   warnings.
#
# * Renamed to `check_downstream()`.
#
# * The requirement format is now "pkg (>= 0.0.0)", consistently with
#   DESCRIPTION fields.


check_downstream <- function(ver, ..., with_rlang = requireNamespace("rlang")) {
  env <- topenv(parent.frame())
  if (!isNamespace(env)) {
    stop("`check_downstream()` must be called from a namespace.", call. = FALSE)
  }
  pkg <- unname(getNamespaceName(env))

  deps <- c(...)
  if (!is.character(deps)) {
    stop("`...` must be strings.", call. = FALSE)
  }
  deps <- .rlang_downstream_parse_deps(deps)

  on_package_load <- function(pkg, expr) {
    if (isNamespaceLoaded(pkg)) {
      expr
    } else {
      thunk <- function(...) expr
      setHook(packageEvent(pkg, "onLoad"), thunk)
    }
  }

  # Must be `lapply()` instead of a `for` loop so that each `dep` is
  # bound to its own closure env
  lapply(deps, function(dep) {
    force(dep)
    on_package_load(
      dep[["pkg"]],
      .rlang_downstream_check(pkg, ver, dep, with_rlang = with_rlang)
    )
  })
}

.rlang_downstream_parse_deps <- function(deps) {
  str_trim <- function(x) {
    sub("^\\s+", "", sub("\\s+$", "", x))
  }
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  deps <- lapply(deps, .rlang_downstream_parse_min_requirement)
  deps
}
.rlang_downstream_parse_min_requirement <- function(dep) {
  if (length(dep) != 2) {
    stop("Parsing error during downstream check.", call. = FALSE)
  }
  is_string <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x)
  }

  parts <- strsplit(dep[[2]], " +")[[1]]

  op <- parts[[1]]
  ver <- parts[[2]]
  stopifnot(is_string(op), is_string(ver))

  if (op != ">=") {
    stop("Can only check `>=` requirements.", call. = FALSE)
  }

  c(pkg = dep[[1]], min = ver)
}

.rlang_downstream_check <- function(pkg, pkg_ver, dep, with_rlang) {
  if (isFALSE(getOption("rlib_downstream_check"))) {
    return()
  }

  dep_pkg <- dep[["pkg"]]
  dep_min <- dep[["min"]]

  curr_ver <- utils::packageVersion(dep_pkg)
  if (curr_ver >= dep_min) {
    return()
  }

  pkg_ver <- utils::packageVersion(pkg)

  header <- sprintf("As of %s %s, %s must be at least version %s.", pkg, pkg_ver, dep_pkg, dep_min)
  info_pkg <- c(x = sprintf("%s %s is too old for %s %s.", dep_pkg, curr_ver, pkg, pkg_ver))
  info_install <- .rlang_downstream_howto_reinstall_msg(dep_pkg)

  if (with_rlang) {
    # Don't use `::` because this is also called from rlang's onLoad
    # hook and exports are not initialised at this point
    warn <- get("warn", envir = asNamespace("rlang"))
    inform <- get("inform", envir = asNamespace("rlang"))
    is_interactive <- get("is_interactive", envir = asNamespace("rlang"))
  } else {
    format_msg <- function(x) paste(x, collapse = "\n")
    warn <- function(msg) warning(format_msg(msg), call. = FALSE)
    inform <- function(msg) message(format_msg(msg))
    is_interactive <- function() interactive() && !isFALSE(getOption("rlang_interactive"))
  }

  if (!is_interactive()) {
    warn(c(header, info_pkg, info_install))
    return()
  }

  prompt <- c(
    "!" = sprintf("Would you like to update the package %s now?", dep_pkg),
    " " = "You will likely need to reload R if you update now."
  )
  inform(c(header, info_pkg, prompt))

  if (utils::menu(c("Yes", "No")) != 1) {
    inform("Set `options(rlib_downstream_check = FALSE)` to disable this prompt.")
    return()
  }

  if (is_installed("pak")) {
    pak::pkg_install(dep_pkg, ask = FALSE)
  } else {
    utils::install.packages(dep_pkg)
  }
}

# Keep in sync with compat-linked-version.R
.rlang_downstream_howto_reinstall_msg <- function(pkg) {
  os <- tolower(Sys.info()[["sysname"]])

  if (os == "windows") {
    url <- "https://github.com/jennybc/what-they-forgot/issues/62"
    c(
      i = sprintf("Please update %s to the latest version.", pkg),
      i = sprintf("Updating packages on Windows requires precautions:\n  <%s>", url)
    )
  } else {
    c(
      i = sprintf("Please update %s with `install.packages(\"%s\")` and restart R.", pkg, pkg)
    )
  }
}

#nocov end
