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
  if (length(from) != 1) {
    abort("`from` must be length one")
  }
  if (length(to) != 1) {
    abort("`to` must be length one")
  }

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

first <- function(x) {
  .subset2(x, 1L)
}
last <- function(x) {
  .subset2(x, length_(x))
}

validate_index <- function(i, n) {
  seq_len(n)[i]
}
