#' Add attributes to an object
#'
#' `set_attrs()` adds, changes, or zaps attributes of objects. Pass a
#' single unnamed `NULL` as argument to zap all attributes. For
#' [uncopyable][is_copyable] types, use `mut_attrs()`.
#'
#' Unlike [structure()], these setters have no special handling of
#' internal attributes names like `.Dim`, `.Dimnames` or `.Names`.
#'
#' @param .x An object to decorate with attributes.
#' @param ... A list of named attributes. These have [explicit
#'   splicing semantics][dots_list]. Pass a single unnamed `NULL` to
#'   zap all attributes from `.x`.
#' @export
#' @examples
#' set_attrs(letters, names = 1:26, class = "my_chr")
#'
#' # Splice a list of attributes:
#' attrs <- list(attr = "attr", names = 1:26, class = "my_chr")
#' obj <- set_attrs(letters, splice(attrs))
#' obj
#'
#' # Zap attributes by passing a single unnamed NULL argument:
#' set_attrs(obj, NULL)
#' set_attrs(obj, !!! list(NULL))
#'
#' # Note that set_attrs() never modifies objects in place:
#' obj
#'
#' # For uncopyable types, mut_attrs() lets you modify in place:
#' env <- env()
#' mut_attrs(env, foo = "bar")
#' env
set_attrs <- function(.x, ...) {
  if (!is_copyable(.x)) {
    abort("`.x` is uncopyable: use `mut_attrs()` to change attributes in place")
  }
  set_attrs_impl(.x, ...)
}
#' @rdname set_attrs
#' @export
mut_attrs <- function(.x, ...) {
  if (is_copyable(.x)) {
    abort("`.x` is copyable: use `set_attrs()` to change attributes without side effect")
  }
  set_attrs_impl(.x, ...)
}
set_attrs_impl <- function(.x, ...) {
  attrs <- dots_list(...)

  # If passed a single unnamed NULL, zap attributes
  if (identical(attrs, set_attrs_null)) {
    attributes(.x) <- NULL
  } else {
    attributes(.x) <- c(attributes(.x), attrs)
  }

  .x
}
set_attrs_null <- list(NULL)
names(set_attrs_null) <- ""

#' Is object named?
#'
#' `is_named()` checks that `x` has names attributes, and that none of
#' the names are missing or empty (`NA` or `""`). `is_dictionaryish()`
#' checks that an object is a dictionary: that it has actual names and
#' in addition that there are no duplicated names. `have_name()`
#' is a vectorised version of `is_named()`.
#'
#' @param x An object to test.
#' @return `is_named()` and `is_dictionaryish()` are scalar predicates
#'   and return `TRUE` or `FALSE`. `have_name()` is vectorised and
#'   returns a logical vector as long as the input.
#' @export
#' @examples
#' # A data frame usually has valid, unique names
#' is_named(mtcars)
#' have_name(mtcars)
#' is_dictionaryish(mtcars)
#'
#' # But data frames can also have duplicated columns:
#' dups <- cbind(mtcars, cyl = seq_len(nrow(mtcars)))
#' is_dictionaryish(dups)
#'
#' # The names are still valid:
#' is_named(dups)
#' have_name(dups)
#'
#'
#' # For empty objects the semantics are slightly different.
#' # is_dictionaryish() returns TRUE for empty objects:
#' is_dictionaryish(list())
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
is_dictionaryish <- function(x) {
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

#' Set names of a vector
#'
#' This is equivalent to [stats::setNames()], with more features and
#' stricter argument checking.
#'
#' @param x Vector to name.
#' @param nm,... Vector of names, the same length as `x`.
#'
#'   You can specify names in the following ways:
#'
#'   * If you do nothing, `x` will be named with itself.
#'
#'   * If `x` already has names, you can provide a function or formula
#'     to transform the existing names. In that case, `...` is passed
#'     to the function.
#'
#'   * If `nm` is `NULL`, the names are removed (if present).
#'
#'   * In all other cases, `nm` and `...` are passed to [chr()]. This
#'     gives implicit splicing semantics: you can pass character
#'     vectors or list of character vectors indistinctly.
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
#' # `...` is passed to the function:
#' set_names(head(mtcars), paste0, "_foo")
set_names <- function(x, nm = x, ...) {
  if (!is_vector(x)) {
    abort("`x` must be a vector")
  }

  if (is_function(nm) || is_formula(nm)) {
    nm <- as_function(nm)
    nm <- nm(names2(x), ...)
  } else if (!is_null(nm)) {
    # Make sure `x` is serialised when no arguments is provided.
    nm <- as.character(nm)
    nm <- chr(nm, ...)
  }

  if (!is_null(nm) && !is_character(nm, length(x))) {
    abort("`nm` must be `NULL` or a character vector the same length as `x`")
  }

  names(x) <- nm
  x
}

#' Get names of a vector
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
  if (type_of(x) == "environment") abort("Use env_names() for environments.")
  nms <- names(x)
  if (is_null(nms)) {
    rep("", length(x))
  } else {
    nms %|% ""
  }
}

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
