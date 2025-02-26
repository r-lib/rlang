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
    abort(sprintf("%s must be length one.", format_arg("from")))
  }
  if (length(to) != 1) {
    abort(sprintf("%s must be length one.", format_arg("to")))
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


#' Poke values into a vector
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' These tools are for R experts only. They copy elements from `y`
#' into `x` by mutation. You should only do this if you own `x`,
#' i.e. if you have created it or if you are certain that it doesn't
#' exist in any other context. Otherwise you might create unintended
#' side effects that have undefined consequences.
#'
#' @param x The destination vector.
#' @param start The index indicating where to start modifying `x`.
#' @param y The source vector.
#' @param from The index indicating where to start copying from `y`.
#' @param n How many elements should be copied from `y` to `x`.
#' @param to The index indicating the end of the range to copy from `y`.
#'
#' @keywords internal
#' @export
vec_poke_n <- function(x, start, y, from = 1L, n = length(y)) {
  stopifnot(
    is_integerish(start),
    is_integerish(from),
    is_integerish(n)
  )
  .Call(ffi_vec_poke_n, x, start, y, from, n)
}
#' @rdname vec_poke_n
#' @export
vec_poke_range <- function(x, start, y, from = 1L, to = length(y) - from + 1L) {
  stopifnot(
    is_integerish(start),
    is_integerish(from),
    is_integerish(to)
  )
  .Call(ffi_vec_poke_range, x, start, y, from, to)
}
