# nocov start --- compat-register --- 2019-06-12 Wed 13:11


s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(method)) {
    method <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(method))

  if (can_s3_register_now(generic, class, method, package)) {
    registerS3method(generic, class, method, envir = asNamespace(package))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      registerS3method(generic, class, method, envir = asNamespace(package))
    }
  )
}

can_s3_register_now <- function(generic, class, method, package) {
  if (!(package %in% loadedNamespaces())) {
    return(FALSE)
  }

  envir <- asNamespace(package)

  # Avoid registration failures during loading (pkgload or regular),
  # only register if generic can be accessed
  exists(generic, envir)
}


# nocov end
