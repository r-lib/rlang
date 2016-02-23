"%||%" <- function(x, y) if(is.null(x)) y else x


#' Is object a formula?
#'
#' @param x Object to test
#' @export
#' @examples
#' is_formula(~ 10)
#' is_formula(10)
is_formula <- function(x) {
  typeof(x) == "language" && inherits(x, "formula")
}

#' Extract the right-hand-side of a formula
#'
#' Throws an error if \code{f} is not a formula, or it's not one-sided.
#'
#' @param f A formula
#' @export
#' @return An atomic vector of length 1, a name, or a call.
#' @examples
#' rhs(~ 1 + 2 + 3)
#' rhs(~ x)
#' rhs(~ "A")
rhs <- function(f) {
  if (!is_formula(f)) {
    stop("`f` is a not a formula", call. = FALSE)
  }

  if (length(f) != 2) {
    stop("`f` is not a one-sided formula", call. = FALSE)
  }

  f[[2]]
}
