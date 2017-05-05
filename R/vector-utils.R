#' Prepend a vector
#'
#' This is a companion to [base::append()] to help merging two lists
#' or atomic vectors. `prepend()` is a clearer semantic signal than
#' `c()` that a vector is to be merged at the beginning of another,
#' especially in a pipe chain.
#'
#' @param x the vector to be modified.
#' @param values to be included in the modified vector.
#' @param before a subscript, before which the values are to be appended.
#' @return A merged vector.
#' @export
#' @examples
#' x <- as.list(1:3)
#'
#' append(x, "a")
#' prepend(x, "a")
#' prepend(x, list("a", "b"), before = 3)
prepend <- function(x, values, before = 1) {
  n <- length(x)
  stopifnot(before > 0 && before <= n)

  if (before == 1) {
    c(values, x)
  } else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}

#' Modify a vector
#'
#' This function merges a list of arguments into a vector. It always
#' returns a list.
#'
#' @param .x A vector to modify.
#' @param ... List of elements to merge into `.x`. Named elements
#'   already existing in `.x` are used as replacements. Elements that
#'   have new or no names are inserted at the end. These dots are
#'   evaluated with [explicit splicing][dots_list].
#' @return A modified vector upcasted to a list.
#' @export
#' @examples
#' modify(c(1, b = 2, 3), 4, b = "foo")
#'
#' x <- list(a = 1, b = 2)
#' y <- list(b = 3, c = 4)
#' modify(x, splice(y))
modify <- function(.x, ...) {
  out <- as.list(.x)
  args <- dots_list(...)

  args_nms <- names(args)
  exists <- have_name(args) & args_nms %in% names(out)

  for (nm in args_nms[exists]) {
    out[[nm]] <- args[[nm]]
  }

  c(out, args[!exists])
}

#' Increasing sequence of integers in an interval
#'
#' These helpers take two endpoints and return the sequence of all
#' integers within that interval. For `seq2_along()`, the upper
#' endpoint is taken from the length of a vector. Unlike
#' `base::seq()`, they return an empty vector if the starting point is
#' a larger integer than the end point.
#'
#' @param from The starting point of the sequence.
#' @param to The end point.
#' @param x A vector whose length is the end point.
#' @return An integer vector containing a strictly increasing
#'   sequence.
#' @export
#' @examples
#' seq2(2, 10)
#' seq2(10, 2)
#' seq(10, 2)
#'
#' seq2_along(10, letters)
seq2 <- function(from, to) {
  if (from > to) {
    int()
  } else {
    seq.int(from, to)
  }
}
#' @rdname seq2
#' @export
seq2_along <- function(from, x) {
  seq2(from, length(x))
}
