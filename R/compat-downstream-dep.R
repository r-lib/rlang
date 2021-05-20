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
# 2020-05-17:
#
# * Added an `info` argument intended to inform users about the
#   consequences of not updating right away.
#
#
# 2020-05-12:
#
# * All packages are now updated at once. The user is not prompted
#   again after accepting or declining to update the packages, even
#   when one of the packages is loaded later on.
#
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


check_downstream <- function(ver,
                             ...,
                             info = NULL,
                             with_rlang = requireNamespace("rlang")) {
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

  checked <- FALSE
  for (dep in deps) {
    on_package_load(
      dep[["pkg"]],
      .rlang_downstream_check(
        pkg,
        ver,
        deps,
        info = info,
        with_rlang = with_rlang
      )
    )
  }
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
  if (length(parts) != 2) {
    stop("Parsing error during downstream check.", call. = FALSE)
  }

  op <- parts[[1]]
  ver <- parts[[2]]
  stopifnot(is_string(op), is_string(ver))

  if (op != ">=") {
    stop("Can only check `>=` requirements.", call. = FALSE)
  }

  c(pkg = dep[[1]], min = ver)
}

.rlang_downstream_check <- function(pkg,
                                    pkg_ver,
                                    deps,
                                    info,
                                    with_rlang = requireNamespace("rlang"),
                                    env = parent.frame()) {
  if (isFALSE(getOption("rlib_downstream_check"))) {
    return()
  }

  if (isTRUE(env$checked)) {
    return(TRUE)
  }

  # Don't ask again
  on.exit(env$checked <- TRUE)

  pkgs <- vapply(deps, `[[`, "", "pkg")
  mins <- vapply(deps, `[[`, "", "min")
  vers <- lapply(pkgs, utils::packageVersion)

  ok <- as.logical(Map(`>=`, vers, mins))

  if (all(ok)) {
    return(TRUE)
  }

  pkgs <- pkgs[!ok]
  mins <- mins[!ok]

  pkgs_quoted <- paste0("`", pkgs, "` (>= ", mins, ")")
  pkgs_enum <- .rlang_downstream_collapse(pkgs_quoted, final = "and")

  n <- length(pkgs)
  if (n == 1) {
    header <- paste0("The package ", pkgs_enum, " is required")
  } else {
    header <- paste0("The packages ", pkgs_enum, " are required")
  }

  header <- sprintf("%s as of %s %s.", header, pkg, pkg_ver)

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
    warn(header)
    return(FALSE)
  }

  if (n == 1) {
    question <- "Would you like to update it now?"
  } else {
    question <- "Would you like to update them now?"
  }

  # Use "i" bullets by default
  if (!is.null(info) && is.null(names(info))) {
    names(info) <- rep("i", length(info))
  }

  prompt <- c(
    "!" = question,
    " " = "You will likely need to restart R if you update now.",
    info
  )
  inform(c(header, prompt))

  if (utils::menu(c("Yes", "No")) != 1) {
    inform("Set `options(rlib_downstream_check = FALSE)` to disable this prompt.")
    return(FALSE)
  }

  if (is_installed("pak")) {
    pak::pkg_install(pkgs, ask = FALSE)
  } else {
    utils::install.packages(pkgs)
  }

  TRUE
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

.rlang_downstream_collapse <- function(x, sep = ", ", final = "or") {
  n <- length(x)

  if (n < 2) {
    return(x)
  }

  n <- length(x)
  head <- x[seq_len(n - 1)]
  last <- x[length(x)]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}

#nocov end
