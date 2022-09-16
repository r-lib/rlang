# nocov start - compat-downstream-deps.R
# Latest version: https://github.com/r-lib/rlang/blob/main/R/compat-downstream-deps.R

# No dependencies but uses rlang and pak if available. In interactive
# sessions the user is prompted to update outdated packages. If they
# choose no, they are informed about the global option
# `rlib_downstream_check` to turn off these prompts. In non
# interactive sessions a warning is issued. This happens when the
# outdated dep is being loaded.

# Changelog:
#
# 2022-01-19:
#
# * Prompt results are no longer cached in the `org:r-lib` search path
#   environment in non-interactive sessions. This is to avoid side
#   effects causing R CMD check failures.
#
#
# 2021-06-08:
#
# * User response is cached in the global env to avoid asking again
#   when session is reloaded.
#
#
# 2021-05-20:
#
# * Fixed issue when downstream package is not installed.
#
#
# 2021-05-17:
#
# * Added an `info` argument intended to inform users about the
#   consequences of not updating right away.
#
#
# 2021-05-12:
#
# * All packages are now updated at once. The user is not prompted
#   again after accepting or declining to update the packages, even
#   when one of the packages is loaded later on.
#
#
# 2021-05-07:
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
                             info = NULL) {
  env <- topenv(parent.frame())
  if (!isNamespace(env)) {
    stop("`check_downstream()` must be called from a namespace.", call. = FALSE)
  }
  pkg <- unname(getNamespaceName(env))

  deps <- c(...)
  if (!is.character(deps)) {
    stop("`...` must be strings.", call. = FALSE)
  }

  deps_key <- paste0(deps, collapse = " ")
  deps <- .rlang_downstream_parse_deps(deps)

  on_package_load <- function(pkg, expr) {
    if (isNamespaceLoaded(pkg)) {
      expr
    } else {
      thunk <- function(...) expr
      setHook(packageEvent(pkg, "onLoad"), thunk)
    }
  }

  is_interactive <- .rlang_downstream_compat("is_interactive")

  if (is_interactive()) {
    cache <- .rlang_downstream_get_cache()
    cache[[pkg]][[deps_key]] <- FALSE
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
        deps_key = deps_key
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
                                    deps_key = as.character(stats::runif(1)),
                                    env = parent.frame()) {
  isFALSE <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
  }

  if (isFALSE(getOption("rlib_downstream_check"))) {
    return(NULL)
  }

  # Check cache in the global environment. This cache gets saved along
  # with the session. This avoids getting repeated checks when session
  # is reloaded, e.g. when revisiting RStudio servers.
  is_interactive <- .rlang_downstream_compat("is_interactive")
  if (is_interactive()) {
    cache <- .rlang_downstream_get_cache()
    if (isTRUE(cache[[pkg]][[deps_key]])) {
      return(NULL)
    }
  }

  # Still check closure env in case the cache in the global
  # environment has been deleted
  if (isTRUE(env$checked)) {
    return(NULL)
  }

  # Don't ask again. Flip now instead of on exit to defensively
  # prevent recursion.
  if (is_interactive()) {
    cache[[pkg]][deps_key] <- list(TRUE)
  }
  env$checked <- TRUE

  pkgs <- vapply(deps, `[[`, "", "pkg")
  mins <- vapply(deps, `[[`, "", "min")

  # Don't use `requireNamespace()` to avoid loading packages
  is_on_disk <- function(pkg) nzchar(system.file(package = pkg))
  on_disk <- vapply(pkgs, is_on_disk, NA)

  pkgs <- pkgs[on_disk]
  mins <- mins[on_disk]

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

  warn <- .rlang_downstream_compat("warn")
  inform <- .rlang_downstream_compat("inform")
  is_interactive <- .rlang_downstream_compat("is_interactive")

  if (!is_interactive() || !is.null(getOption("rlang:::no_downstream_prompt"))) {
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
    pkg_install <- get(envir = asNamespace("pak"), "pkg_install")
    pkg_install(pkgs, ask = FALSE)
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

.rlang_downstream_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    # Make sure rlang knows about "x" and "i" bullets
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}

.rlang_downstream_get_cache <- function() {
  if (!"org:r-lib" %in% search()) {
    do.call(
      attach,
      list(
        list(),
        pos = length(search()),
        name = "org:r-lib"
      )
    )
  }

  cache_env <- as.environment("org:r-lib")
  check_cache_name <- "rlang_downstream_check"
  cache <- cache_env[[check_cache_name]]

  if (is.null(cache)) {
    cache <- new.env(parent = emptyenv())
    cache_env[[check_cache_name]] <- cache
  }

  cache
}


#nocov end
