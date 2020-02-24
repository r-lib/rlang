# nocov start --- compat-downstream-deps --- 2020-02-24 Mon 15:57 CET


check_downstream_deps <- local({

  # Keep in sync with compat-linked-version.R
  howto_reinstall_msg <- function(pkg) {
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

  is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

  check_downstream_dep <- function(pkg, dep_pkg, dep_data, with_rlang) {
    min <- dep_data[["min"]]
    from <- dep_data[["from"]]
    stopifnot(
      is_string(min),
      is_string(from)
    )

    ver <- utils::packageVersion(dep_pkg)
    if (ver >= min) {
      return()
    }

    rlang_ver <- utils::packageVersion("rlang")

    header <- sprintf("As of rlang %s, %s must be at least version %s.", from, dep_pkg, min)
    body <- c(
      x = sprintf("%s %s is too old for rlang %s.", dep_pkg, ver, rlang_ver),
      howto_reinstall_msg(dep_pkg)
    )

    if (with_rlang) {
      body <- rlang::format_error_bullets(body)
      msg <- paste(c(header, body), collapse = "\n")
      rlang::warn(msg)
    } else {
      body <- paste0("* ", body)
      msg <- paste(c(header, body), collapse = "\n")
      warning(msg, call. = FALSE)
    }
  }

  on_package_load <- function(pkg, expr) {
    if (isNamespaceLoaded(pkg)) {
      expr
    } else {
      thunk <- function(...) expr
      setHook(packageEvent(pkg, "onLoad"), thunk)
    }
  }

  function(pkg, ..., with_rlang = requireNamespace("rlang")) {
    deps <- list(...)
    nms <- names(deps)

    if (is.null(nms)) {
      stop("Downstream dependencies should be named.", call. = FALSE)
    }

    Map(
      function(dep_pkg, dep_data) {
        force(dep_data)
        on_package_load(dep_pkg, check_downstream_dep(
          pkg,
          dep_pkg,
          dep_data,
          with_rlang = with_rlang
        ))
      },
      nms,
      deps
    )
  }
})


#nocov end
