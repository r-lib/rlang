# nocov start - compat-downstream.R
# Latest version: https://github.com/r-lib/rlang/blob/master/R/compat-downstream-deps.R

# Changelog:
#
# 2020-05-07:
# * Renamed to `check_downstream()`.
#
# * The requirement format is now "pkg (>= 0.0.0)", consistently with
#   DESCRIPTION fields.


check_downstream <- function(ver, ..., with_rlang = requireNamespace("rlang")) {
  env <- topenv(parent.frame())
  if (!isNamespace(env)) {
    stop("`check_downstream()` must be called from a namespace.", call. = FALSE)
  }
  pkg <- getNamespaceName(env)

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

  for (dep in deps) {
    on_package_load(
      dep[["pkg"]],
      .rlang_downstream_check(pkg, ver, dep, with_rlang = with_rlang)
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

  op <- parts[[1]]
  ver <- parts[[2]]
  stopifnot(is_string(op), is_string(ver))

  if (op != ">=") {
    stop("Can only check `>=` requirements.", call. = FALSE)
  }

  c(pkg = dep[[1]], min = ver)
}

.rlang_downstream_check <- function(pkg, pkg_ver, dep, with_rlang) {
  dep_pkg <- dep[["pkg"]]
  dep_min <- dep[["min"]]

  curr_ver <- utils::packageVersion(dep_pkg)
  if (curr_ver >= dep_min) {
    return()
  }

  pkg_ver <- utils::packageVersion(pkg)

  header <- sprintf("As of %s %s, %s must be at least version %s.", pkg, pkg_ver, dep_pkg, dep_min)
  body <- c(
    x = sprintf("%s %s is too old for %s %s.", dep_pkg, curr_ver, pkg, pkg_ver),
    .rlang_downstream_howto_reinstall_msg(dep_pkg)
  )

  if (with_rlang) {
    # Don't use `::` because this is also called from rlang's onLoad
    # hook and exports are not initialised at this point
    warn <- get("warn", envir = asNamespace("rlang"))
    warn(c(header, body))
  } else {
    body <- paste0("* ", body)
    msg <- paste(c(header, body), collapse = "\n")
    warning(msg, call. = FALSE)
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
