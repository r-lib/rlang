#' @rdname eval_tidy
#' @param x An object for which you want to find associated data.
#' @param lookup_msg An error message when your data source is
#'   accessed inappropriately (by position rather than name).
#' @export
dictionary <- function(x, lookup_msg = NULL) {
  UseMethod("dictionary")
}
#' @export
dictionary.default <- function(x, lookup_msg = NULL) {
  x <- discard_unnamed(x)
  if (!is_dictionary(x)) {
    abort("Data source must be a dictionary")
  }
  new_dictionary(as.list(x), lookup_msg)
}
#' @export
dictionary.dictionary <- function(x, lookup_msg = NULL) {
  classes <- class(x)
  x <- unclass(x)
  x$lookup_msg <- lookup_msg %||% x$lookup_msg
  structure(x, class = classes)
}
#' @export
dictionary.NULL <- function(x, lookup_msg = NULL) {
  dictionary(list(), lookup_msg = lookup_msg)
}
#' @export
dictionary.environment <- function(x, lookup_msg = NULL) {
  lookup_msg <- lookup_msg %||% "Object '%s' not found in environment"
  if (!identical(x, global_env())) {
    x <- env_clone(x)
  }
  new_dictionary(x, lookup_msg)
}
#' @export
dictionary.data.frame <- function(x, lookup_msg = NULL) {
  lookup_msg <- lookup_msg %||% "Variable '%s' not found in data"
  new_dictionary(x, lookup_msg)
}

new_dictionary <- function(x, lookup_msg) {
  msg <- lookup_msg %||% "Object '%s' not found in pronoun"
  class <- "dictionary"
  structure(list(src = x, lookup_msg = msg), class = class)
}

#' @export
`$.dictionary` <- function(x, name) {
  src <- .subset2(x, "src")
  if (!has_binding(src, name)) {
    abort(sprintf(.subset2(x, "lookup_msg"), name))
  }
  src[[name]]
}
#' @export
`[[.dictionary` <- function(x, i, ...) {
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
names.dictionary <- function(x) {
  names(unclass(x)$src)
}
#' @export
length.dictionary <- function(x) {
  length(unclass(x)$src)
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
print.dictionary <- function(x, ...) {
  print(unclass_src(x), ...)
}
#' @importFrom utils str
#' @export
str.dictionary <- function(object, ...) {
  str(unclass_src(object), ...)
}
unclass_src <- function(x) {
  i <- match("dictionary", class(x))
  class(x) <- class(x)[-i]
  x
}
