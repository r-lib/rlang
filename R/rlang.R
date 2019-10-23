#' @useDynLib rlang, .registration = TRUE
NULL

# For cnd.R
is_same_body <- NULL


downstream_deps <- list(
  dplyr = c(min = "0.8.0", from = "0.4.0")
)

check_downstream_dep <- function(dep, pkg) {
  if (!isNamespaceLoaded(pkg)) {
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) check_downstream_dep(dep, pkg)
    )
    return()
  }

  min <- dep[["min"]]
  from <- dep[["from"]]
  stopifnot(
    !is_null(min),
    !is_null(from)
  )

  ver <- utils::packageVersion(pkg)
  if (ver >= min) {
    return()
  }

  rlang_ver <- utils::packageVersion("rlang")

  msg <- c(
    sprintf("As of rlang %s, %s must be at least version %s.", from, pkg, min),
    x = sprintf("%s %s is too old for rlang %s.", pkg, ver, rlang_ver)
  )

  os <- tolower(Sys.info()[["sysname"]])
  if (os == "windows") {
    url <- "https://github.com/jennybc/what-they-forgot/issues/62"
    howto <- c(
      i = sprintf("Please update %s to the latest version.", pkg),
      i = sprintf("Updating packages on Windows requires precautions:\n  <%s>", url)
    )
  } else {
    howto <- c(
      i = sprintf("Please update %s with `install.packages(\"%s\")`.", pkg, pkg)
    )
  }
  msg <- c(msg, howto)

  warn(msg)
}

.onLoad <- function(lib, pkg) {
  if (getRversion() < "3.5") {
    is_same_body <<- function(x, y) identical(x, y)
  } else {
    is_same_body <<- is_reference
  }

  .Call(r_init_library)
  .Call(rlang_library_load)

  s3_register("pillar::pillar_shaft", "quosures", pillar_shaft.quosures)
  s3_register("pillar::type_sum", "quosures", type_sum.quosures)

  map2(downstream_deps, names(downstream_deps), check_downstream_dep)
}
.onUnload <- function(lib) {
  .Call(rlang_library_unload)
}
