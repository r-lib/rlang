complain <- function(x) {
  if (is.environment(x)) {
    x <- clone_env(x)
  }

  structure(x, class = "complain")
}

clone_env <- function(x) {
  list2env(as.list(x, all.names = TRUE), parent = parent.env(x))
}

#' @export
`$.complain` <- function(x, name) {
  if (!has_name(x, name)) {
    stop("object '", name, "' not found", call. = FALSE)
  }
  x[[name]]
}

#' @export
`[[.complain` <- function(x, i, ...) {
  if (!is.character(i) || length(i) != 1) {
    stop("Must subset with a string", call. = FALSE)
  }
  if (!has_name(x, i)) {
    stop("object '", i, "' not found", call. = FALSE)
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
  exists(x, name, inherits = FALSE)
}
