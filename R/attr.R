#' Is object named?
#'
#' \code{is_named()} checks that \code{x} has names attributes, and
#' that none of the names are missing or empty (\code{NA} or
#' \code{""}). \code{is_dictionary()} checks that an object is a
#' dictionary: that it has actual names and in addition that there are
#' no duplicated names. \code{has_names()} is a vectorised version of
#' \code{is_named()}.
#'
#' @param x An object to test.
#' @return \code{is_named()} and \code{is_dictionary()} are scalar
#'   predicates and return \code{TRUE} or \code{FALSE}.
#'   \code{has_names()} is vectorised and returns a logical vector as
#'   long as the input.
#' @export
#' @examples
#' # A data frame usually has valid, unique names
#' is_named(mtcars)
#' has_names(mtcars)
#' is_dictionary(mtcars)
#'
#' # But data frames can also have duplicated columns:
#' dups <- cbind(mtcars, cyl = seq_len(nrow(mtcars)))
#' is_dictionary(dups)
#'
#' # The names are still valid:
#' is_named(dups)
#' has_names(dups)
#'
#'
#' # For empty objects the semantics are slightly different.
#' # is_dictionary() returns TRUE for empty objects:
#' is_dictionary(list())
#'
#' # But is_named() will only return TRUE if there is a names
#' # attribute (a zero-length character vector in this case):
#' x <- set_names(list(), character(0))
#' is_named(x)
#'
#'
#' # Empty and missing names are invalid:
#' invalid <- dups
#' names(invalid)[2] <- ""
#' names(invalid)[5] <- NA
#'
#' # is_named() performs a global check while has_names() can show you
#' # where the problem is:
#' is_named(invalid)
#' has_names(invalid)
#'
#' # has_names() will work even with vectors that don't have a names
#' # attribute:
#' has_names(letters)
is_named <- function(x) {
  nms <- names(x)

  if (is_null(nms)) {
    return(FALSE)
  }

  if (any(nms == "" | is.na(nms))) {
    return(FALSE)
  }

  TRUE
}

#' @rdname is_named
#' @export
is_dictionary <- function(x) {
  if (!length(x)) {
    return(!is.null(x))
  }

  is_named(x) && !any(duplicated(names(x)))
}

#' @rdname is_named
#' @export
has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

#' Set names of a vector.
#'
#' This is a snake case wrapper for \code{\link[stats]{setNames}},
#' with tweaked defaults, and stricter argument checking.
#'
#' @param x Vector to name.
#' @param nm Vector of names, the same length as \code{x}.
#' @export
#' @examples
#' set_names(1:4, c("a", "b", "c", "d"))
#'
#' # If the second argument is ommitted a vector is named with itself
#' set_names(letters[1:5])
set_names <- function(x, nm = x) {
  if (!is_vector(x)) {
    stop("`x` must be a vector", call. = FALSE)
  }
  if (!is_null(nm) && length(x) != length(nm)) {
    stop("`x` and `nm` must be the same length", call. = FALSE)
  }

  names(x) <- nm
  x
}

#' Get names of a vector.
#'
#' This names getter always returns a character vector, even when an
#' object does not have a \code{names} attribute. In this case, it
#' returns a vector of empty names \code{""}. It also standardises
#' missing names to \code{""}.
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
  nms <- names(x)
  if (is_null(nms)) {
    rep("", length(x))
  } else {
    nms %|% ""
  }
}

#' @useDynLib rlang length__
length_ <- function(x) {
  .Call(length__, x)
}

#' How long is an object?
#'
#' This is a function for the common task of testing the length of an
#' object. It checks the length of an object in a non-generic way:
#' \code{\link[base]{length}()} methods are ignored.
#'
#' @param x A R object.
#' @param n A specific length to test \code{x} with. If \code{NULL},
#'   \code{has_length()} returns \code{TRUE} if \code{x} has length
#'   greater than zero, and \code{FALSE} otherwise.
#' @export
#' @examples
#' has_length(list())
#' has_length(list(), 0)
#'
#' has_length(letters)
#' has_length(letters, 20)
#' has_length(letters, 26)
has_length <- function(x, n = NULL) {
  len <- .Call(length__, x)

  if (is_null(n)) {
    as.logical(len)
  } else {
    len == n
  }
}

#' Add attributes to an object.
#'
#' @param .x An object to decorate with attributes.
#' @param ...,.attrs A list of named attributes.
#' @export
#' @examples
#' with_attributes(letters, class = "my_chr", names = 1:26)
#'
#' attrs <- list(class = "my_chr", names = 1:26)
#' with_attributes(letters, .attrs = attrs)
with_attributes <- function(.x, ..., .attrs = list()) {
  invoke("structure", c(list(.Data = .x, ...), .attrs))
}
