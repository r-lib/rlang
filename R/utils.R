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

rhs <- function(f) {
  if (!is_formula(f)) {
    stop("`f` is a not a formula", call. = FALSE)
  }

  if (length(f) != 2) {
    stop("`f` is not a one-sided formula", call. = FALSE)
  }

  f[[2]]
}
