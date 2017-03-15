#' Is object named?
#'
#' `is_named()` checks that `x` has names attributes, and that none of
#' the names are missing or empty (`NA` or `""`). `is_dictionary()`
#' checks that an object is a dictionary: that it has actual names and
#' in addition that there are no duplicated names. `have_name()`
#' is a vectorised version of `is_named()`.
#'
#' @param x An object to test.
#' @return `is_named()` and `is_dictionary()` are scalar predicates
#'   and return `TRUE` or `FALSE`. `have_name()` is vectorised and
#'   returns a logical vector as long as the input.
#' @export
#' @examples
#' # A data frame usually has valid, unique names
#' is_named(mtcars)
#' have_name(mtcars)
#' is_dictionary(mtcars)
#'
#' # But data frames can also have duplicated columns:
#' dups <- cbind(mtcars, cyl = seq_len(nrow(mtcars)))
#' is_dictionary(dups)
#'
#' # The names are still valid:
#' is_named(dups)
#' have_name(dups)
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
#' # is_named() performs a global check while have_name() can show you
#' # where the problem is:
#' is_named(invalid)
#' have_name(invalid)
#'
#' # have_name() will work even with vectors that don't have a names
#' # attribute:
#' have_name(letters)
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
have_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

#' Does an object have an element with this name?
#'
#' This function returns a logical value that indicates if a data frame or
#' another named object contains an element with a specific name.
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

#' Set names of a vector.
#'
#' This is a snake case wrapper for [stats::setNames()], with tweaked
#' defaults, and stricter argument checking.
#'
#' @param x Vector to name.
#' @param nm Vector of names, the same length as `x`.
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
#' object does not have a `names` attribute. In this case, it returns
#' a vector of empty names `""`. It also standardises missing names to
#' `""`.
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

#' @useDynLib rlang rlang_length
length_ <- function(x) {
  .Call(rlang_length, x)
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
#' @examples
#' has_length(list())
#' has_length(list(), 0)
#'
#' has_length(letters)
#' has_length(letters, 20)
#' has_length(letters, 26)
has_length <- function(x, n = NULL) {
  len <- .Call(rlang_length, x)

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

zap_attributes <- function(x) {
  switch_type(x,
    NULL = ,
    char = ,
    symbol = ,
    environment = abort(paste0(
      "Cannot change attributes of uncopyable type `", type_of(x), "`"
    ))
  )
  attributes(x) <- NULL
  x
}
