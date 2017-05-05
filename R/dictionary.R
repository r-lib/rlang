#' Create a dictionary
#'
#' @description
#'
#' Dictionaries are a concept of types modelled after R
#' environments. Dictionaries are containers of R objects that:
#'
#' - Contain uniquely named objects.
#'
#' - Can only be indexed by name. They must implement the extracting
#'   operators `$` and `[[`. The latter returns an error when indexed
#'   by position because dictionaries are not vectors (they are
#'   unordered).
#'
#' - Report a clear error message when asked to extract a name that
#'   does not exist. This error message can be customised with the
#'   `lookup_msg` constructor argument.
#'
#' @details
#'
#' Dictionaries are used within the tidy evaluation framework for
#' creating pronouns that can be explicitly referred to from captured
#' code. See [eval_tidy()].
#'
#' @param x An object for which you want to find associated data.
#' @param lookup_msg An error message when your data source is
#'   accessed inappropriately (by position rather than name).
#' @param read_only Whether users can replace elements of the
#'   dictionary.
#' @name dictionary
#' @export
as_dictionary <- function(x, lookup_msg = NULL, read_only = FALSE) {
  UseMethod("as_dictionary")
}
#' @export
as_dictionary.default <- function(x, lookup_msg = NULL, read_only = FALSE) {
  x <- discard_unnamed(x)
  if (!is_dictionaryish(x)) {
    abort("Data source must be a dictionary")
  }
  new_dictionary(as.list(x), lookup_msg, read_only)
}
#' @export
as_dictionary.dictionary <- function(x, lookup_msg = NULL, read_only = FALSE) {
  dict <- unclass_dict(x)
  dict$lookup_msg <- lookup_msg %||% x$lookup_msg
  dict$read_only <- read_only
  set_attrs(dict, class = class(x))
}
#' @export
as_dictionary.NULL <- function(x, lookup_msg = NULL, read_only = FALSE) {
  new_dictionary(list(), lookup_msg, read_only)
}
#' @export
as_dictionary.environment <- function(x, lookup_msg = NULL, read_only = FALSE) {
  lookup_msg <- lookup_msg %||% "Object `%s` not found in environment"
  new_dictionary(x, lookup_msg, read_only)
}
#' @export
as_dictionary.data.frame <- function(x, lookup_msg = NULL, read_only = FALSE) {
  lookup_msg <- lookup_msg %||% "Column `%s` not found in data"
  new_dictionary(x, lookup_msg, read_only)
}

new_dictionary <- function(x, lookup_msg, read_only) {
  .Call(rlang_new_dictionary, x, lookup_msg, read_only)
}

#' @rdname dictionary
#' @export
is_dictionary <- function(x) {
  inherits(x, "dictionary")
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
  if (!is_string(i)) {
    abort("Must subset with a string")
  }
  src <- .subset2(x, "src")
  if (!has_binding(src, i)) {
    abort(sprintf(.subset2(x, "lookup_msg"), i))
  }
  src[[i, ...]]
}

#' @export
`$<-.dictionary` <- function(x, i, value) {
  dict <- unclass_dict(x)

  if (dict$read_only) {
    abort("Can't modify read-only dictionary")
  }

  dict$src[[i]] <- value
  set_attrs(dict, class = class(x))
}
#' @export
`[[<-.dictionary` <- function(x, i, value) {
  dict <- unclass_dict(x)

  if (dict$read_only) {
    abort("Can't modify read-only dictionary")
  }
  if (!is_string(i)) {
    abort("Must subset with a string")
  }

  dict$src[[i]] <- value
  set_attrs(dict, class = class(x))
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

#' @export
print.dictionary <- function(x, ...) {
  src <- unclass_dict(x)$src
  objs <- glue_countable(length(src), "object")
  cat(paste0("# A dictionary: ", objs, "\n"))
  invisible(x)
}
#' @importFrom utils str
#' @export
str.dictionary <- function(object, ...) {
  str(unclass_dict(object)$src, ...)
}

glue_countable <- function(n, str) {
  if (n == 1) {
    paste0(n, " ", str)
  } else {
    paste0(n, " ", str, "s")
  }
}
# Unclassing before print() or str() is necessary because default
# methods index objects with integers
unclass_dict <- function(x) {
  i <- match("dictionary", class(x))
  class(x) <- class(x)[-i]
  x
}
