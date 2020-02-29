# nocov start --- compat-linked-version --- 2020-02-24 Mon 13:05 CET


check_linked_version <- local({

  # Keep in sync with compat-downstream-deps.R
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

  function(pkg, with_rlang = requireNamespace("rlang")) {
    ver <- utils::packageVersion(pkg)

    ns <- asNamespace(pkg)
    linked_ver_ptr <- ns[[paste0(pkg, "_linked_version")]]
    if (is.null(linked_ver_ptr)) {
      linked_ver <- ""
    } else {
      # Construct call to avoid NOTE when argument to `.Call()` is not
      # statically analysable
      linked_ver <- do.call(".Call", list(linked_ver_ptr))
    }

    if (nzchar(linked_ver) && ver == linked_ver) {
      return(invisible(NULL))
    }

    header <- sprintf("The %s package is not properly installed.", pkg)

    if (nzchar(linked_ver)) {
      msg <- c(x = sprintf(
        "The DLL version (%s) does not correspond to the package version (%s).",
        linked_ver,
        ver
      ))
    } else {
      # Package does not have a version pointer. This happens when DLL
      # updating fails for the first version that includes the pointer.
      msg <- c(x = "The DLL version does not correspond to the package version.")
    }

    msg <- c(msg, howto_reinstall_msg(pkg))

    if (with_rlang) {
      msg <- paste(header, rlang::format_error_bullets(msg), sep = "\n")
      rlang::abort(msg)
    } else {
      msg <- paste(c(header, msg), collapse = "\n")
      stop(msg, call. = FALSE)
    }
  }
})


# nocov end
