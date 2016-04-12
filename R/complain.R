complain <- function(x, message = "object '%s' not found") {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.environment(x)) {
    x <- clone_env(x)
  }

  structure(x, message = message, class = c("complain", class(x)))
}

clone_env <- function(x) {
  list2env(as.list(x, all.names = TRUE), parent = parent.env(x))
}

#' @export
`$.complain` <- function(x, name) {
  if (!has_name(x, name)) {
    stop(sprintf(attr(x, "message"), name), call. = FALSE)
  }
  x[[name]]
}

#' @export
`[[.complain` <- function(x, i, ...) {
  if (!is.character(i) || length(i) != 1) {
    stop("Must subset with a string", call. = FALSE)
  }
  if (!has_name(x, i)) {
    stop(sprintf(attr(x, "message"), i), call. = FALSE)
  }
  NextMethod()
}
has_name <- function(x, name) {
  UseMethod("has_name")
}
#' @export
has_name.default <- function(x, name) {
  name %in% names(x)
}
#' @export
has_name.environment <- function(x, name) {
  exists(name, envir = x, inherits = FALSE)
}
