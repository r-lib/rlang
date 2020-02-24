# nocov start --- compat-downstream-deps --- 2020-02-24 Mon 12:59 CET


check_downstream_deps <- local({

  check_downstream_dep <- function(dep, pkg) {
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
        i = sprintf("Please update %s with `install.packages(\"%s\")` and restart R.", pkg, pkg)
      )
    }
    msg <- c(msg, howto)

    warn(msg)
  }

  on_package_load <- function(pkg, expr) {
    if (isNamespaceLoaded(pkg)) {
      expr
    } else {
      thunk <- function(...) expr
      setHook(packageEvent(pkg, "onLoad"), thunk)
    }
  }

  function(deps) {
    Map(
      function(dep, pkg) {
        force(dep)
        on_package_load(pkg, check_downstream_dep(dep, pkg))
      },
      deps,
      names(deps)
    )
  }
})


#nocov end
