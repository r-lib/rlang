#' @rdname tidy_eval
#' @param x An object for which you want to find associated data.
#' @param lookup_msg An error message when your data source is
#'   accessed inappropriately (by position rather than name).
#' @export
data_source <- function(x, lookup_msg = NULL) {
  UseMethod("data_source")
}
#' @export
data_source.default <- function(x, lookup_msg = NULL) {
  if (!is_dictionary(x)) {
    abort("Data source must be a dictionary")
  }
  new_data_source(as.list(x), lookup_msg)
}
#' @export
data_source.data_source <- function(x, lookup_msg = NULL) {
  classes <- class(x)
  x <- unclass(x)
  x$lookup_msg <- lookup_msg %||% x$lookup_msg
  structure(x, class = classes)
}
#' @export
data_source.NULL <- function(x, lookup_msg = NULL) {
  data_source(list(), lookup_msg = lookup_msg)
}
#' @export
data_source.environment <- function(x, lookup_msg = NULL) {
  lookup_msg <- lookup_msg %||% "Object '%s' not found in environment"
  if (!identical(x, global_env())) {
    x <- env_clone(x)
  }
  new_data_source(x, lookup_msg)
}
#' @export
data_source.data.frame <- function(x, lookup_msg = NULL) {
  lookup_msg <- lookup_msg %||% "Variable '%s' not found in data"
  new_data_source(x, lookup_msg)
}

new_data_source <- function(x, lookup_msg) {
  msg <- lookup_msg %||% "Object '%s' not found in pronoun"
  class <- c("data_source", class(x))
  structure(list(src = x, lookup_msg = msg), class = class)
}

#' @export
`$.data_source` <- function(x, name) {
  src <- .subset2(x, "src")
  if (!has_binding(src, name)) {
    abort(sprintf(.subset2(x, "lookup_msg"), name))
  }
  src[[name]]
}
#' @export
`[[.data_source` <- function(x, i, ...) {
  if (!is_scalar_character(i)) {
    abort("Must subset with a string")
  }
  src <- .subset2(x, "src")
  if (!has_binding(src, i)) {
    abort(sprintf(.subset2(x, "lookup_msg"), i))
  }
  src[[i, ...]]
}
#' @export
names.data_source <- function(x) {
  names(x$src)
}
#' @export
length.data_source <- function(x) {
  length(x$src)
}

has_binding <- function(x, name) {
  UseMethod("has_binding")
}
#' @export
has_binding.default <- function(x, name) {
  name %in% names(x)
}
#' @export
has_binding.environment <- function(x, name) {
  env_has(x, name)
}

# Unclassing before print() or str() is necessary because default
# methods index objects with integers

#' @export
print.data_source <- function(x, ...) {
  print(unclass_src(x), ...)
}
#' @importFrom utils str
#' @export
str.data_source <- function(object, ...) {
  str(unclass_src(object), ...)
}
unclass_src <- function(x) {
  i <- match("data_source", class(x))
  class(x) <- class(x)[-i]
  x
}
