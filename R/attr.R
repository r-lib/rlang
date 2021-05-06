structure2 <- function(.x, ...) {
  exec("structure", .Data = .x, ...)
}

set_class <- function(x, class) {
  attr(x, "class") <- class
  x
}

#' Is object named?
#'
#' `is_named()` checks that none of the names are missing or empty
#' (`NA` or `""`). `have_name()` is a vectorised version of
#' `is_named()`.
#'
#' @param x A vector to test.
#' @return `is_named()` is a scalar predicate that returns `TRUE` or
#'   `FALSE`. `have_name()` is vectorised and returns a logical vector
#'   as long as the input.
#'
#' @details
#' `is_named()` always returns `TRUE` for empty vectors because it
#'  tests for the property that each element of a vector is named
#'  rather than the presence of a `names` attribute.
#'
#' @examples
#' # is_named() is a scalar predicate about the whole vector of names:
#' is_named(c(a = 1, b = 2))
#' is_named(c(a = 1, 2))
#'
#' # have_name() is a vectorised predicate
#' have_name(c(a = 1, b = 2))
#' have_name(c(a = 1, 2))
#'
#' # Empty and missing names are treated as invalid:
#' invalid <- set_names(letters[1:5])
#' names(invalid)[1] <- ""
#' names(invalid)[3] <- NA
#'
#' is_named(invalid)
#' have_name(invalid)
#'
#' # A data frame normally has valid, unique names
#' is_named(mtcars)
#' have_name(mtcars)
#'
#' # A matrix usually doesn't because the names are stored in a
#' # different attribute
#' mat <- matrix(1:4, 2)
#' colnames(mat) <- c("a", "b")
#' is_named(mat)
#' names(mat)
#' @export
is_named <- function(x) {
  nms <- names(x)

  if (is_null(nms)) {
    # Empty vectors are always named
    return(!length(x))
  }

  if (any(detect_void_name(nms))) {
    return(FALSE)
  }

  TRUE
}

#' @rdname is_named
#' @export
have_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !detect_void_name(nms)
  }
}
detect_named <- have_name

detect_void_name <- function(x) {
  x == "" | is.na(x)
}

#' Does an object have an element with this name?
#'
#' This function returns a logical value that indicates if a data
#' frame or another named object contains an element with a specific
#' name. Note that `has_name()` only works with vectors. For instance,
#' environments need the specialised function [env_has()].
#'
#' Unnamed objects are treated as if all names are empty strings. `NA`
#' input gives `FALSE` as output.
#'
#' @param x A data frame or another named object
#' @param name Element name(s) to check
#' @return A logical vector of the same length as `name`
#' @examples
#' has_name(iris, "Species")
#' has_name(mtcars, "gears")
#' @export
has_name <- function(x, name) {
  name %in% names2(x)
}

#' Set names of a vector
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This is equivalent to [stats::setNames()], with more features and
#' stricter argument checking.
#'
#'
#' @section Life cycle:
#'
#' `set_names()` is stable and exported in purrr.
#'
#' @param x Vector to name.
#' @param nm,... Vector of names, the same length as `x`. If length 1,
#'   `nm` is recycled to the length of `x` following the recycling
#'   rules of the tidyverse..
#'
#'   You can specify names in the following ways:
#'
#'   * If not supplied, `x` will be named to `as.character(x)`.
#'
#'   * If `x` already has names, you can provide a function or formula
#'     to transform the existing names. In that case, `...` is passed
#'     to the function.
#'
#'   * Otherwise if `...` is supplied, `x` is named to `c(nm, ...)`.
#'
#'   * If `nm` is `NULL`, the names are removed (if present).
#' @export
#' @examples
#' set_names(1:4, c("a", "b", "c", "d"))
#' set_names(1:4, letters[1:4])
#' set_names(1:4, "a", "b", "c", "d")
#'
#' # If the second argument is ommitted a vector is named with itself
#' set_names(letters[1:5])
#'
#' # Alternatively you can supply a function
#' set_names(1:10, ~ letters[seq_along(.)])
#' set_names(head(mtcars), toupper)
#'
#' # If the input vector is unnamed, it is first named after itself
#' # before the function is applied:
#' set_names(letters, toupper)
#'
#' # `...` is passed to the function:
#' set_names(head(mtcars), paste0, "_foo")
#'
#' # If length 1, the second argument is recycled to the length of the first:
#' set_names(1:3, "foo")
#' set_names(list(), "")
set_names <- function(x, nm = x, ...) {
  mold <- x
  .Call(ffi_set_names, x, mold, nm, environment())
}

#' Get names of a vector
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This names getter always returns a character vector, even when an
#' object does not have a `names` attribute. In this case, it returns
#' a vector of empty names `""`. It also standardises missing names to
#' `""`.
#'
#'
#' @section Life cycle:
#'
#' `names2()` is stable.
#'
#' @param x A vector.
#' @export
#' @examples
#' names2(letters)
#'
#' # It also takes care of standardising missing names:
#' x <- set_names(1:3, c("a", NA, "b"))
#' names2(x)
names2 <- function(x) {
  .Call(ffi_names2, x, environment())
}

# Avoids `NA` names on subset-assign with unnamed vectors
`names2<-` <- function(x, value) {
  if (is_null(names(x))) {
    names(x) <- names2(x)
  }
  names(x) <- value
  x
}

length_ <- function(x) {
  .Call(ffi_length, x)
}

#' How long is an object?
#'
#' This is a function for the common task of testing the length of an
#' object. It checks the length of an object in a non-generic way:
#' [base::length()] methods are ignored.
#'
#' @param x A R object.
#' @param n A specific length to test `x` with. If `NULL`,
#'   `has_length()` returns `TRUE` if `x` has length greater than
#'   zero, and `FALSE` otherwise.
#' @export
#' @keywords internal
#' @examples
#' has_length(list())
#' has_length(list(), 0)
#'
#' has_length(letters)
#' has_length(letters, 20)
#' has_length(letters, 26)
has_length <- function(x, n = NULL) {
  len <- .Call(ffi_length, x)

  if (is_null(n)) {
    as.logical(len)
  } else {
    len == n
  }
}

poke_attributes <- function(x, attrs) {
  .Call(ffi_poke_attrib, x, attrs)
}

#' Zap source references
#'
#' @description
#'
#' There are a number of situations where R creates source references:
#'
#' - Reading R code from a file with `source()` and `parse()` might save
#'   source references inside calls to `function` and `{`.
#' - [sys.call()] includes a source reference if possible.
#' - Creating a closure stores the source reference from the call to
#'   `function`, if any.
#'
#' These source references take up space and might cause a number of
#' issues. `zap_srcref()` recursively walks through expressions and
#' functions to remove all source references.
#'
#' @param x An R object. Functions and calls are walked recursively.
#'
#' @export
zap_srcref <- function(x) {
  if (is_closure(x)) {
    body(x) <- zap_srcref(body(x))
    return(x)
  }
  if (!is_call(x)) {
    return(x)
  }

  x <- duplicate(x, shallow = TRUE)

  if (!is_null(obj_attrib(x))) {
    attr(x, "srcref") <- NULL
    attr(x, "wholeSrcref") <- NULL
    attr(x, "srcfile") <- NULL
  }
  if (is_call(x, "function")) {
    node <- node_get(x, 3)
    if (!is_null(node)) {
      node_poke_cdr(node, NULL)
    }
  }

  node <- x
  while (!is_null(node)) {
    node_poke_car(node, zap_srcref(node_car(node)))
    node <- node_cdr(node)
  }

  x
}
