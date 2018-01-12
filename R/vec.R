#' Poke values into a vector
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
vec_poke_n <- function(x, start, y,
                       from = 1L,
                       n = length(y)) {
  stopifnot(
    is_integerish(start),
    is_integerish(from),
    is_integerish(n)
  )
  .Call(rlang_vec_poke_n, x, start, y, from, n)
}
#' @rdname vec_poke_n
#' @export
vec_poke_range <- function(x, start, y,
                           from = 1L,
                           to = length(y) - from + 1L) {
  stopifnot(
    is_integerish(start),
    is_integerish(from),
    is_integerish(to)
  )
  .Call(rlang_vec_poke_range, x, start, y, from, to)
}
