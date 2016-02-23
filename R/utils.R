"%||%" <- function(x, y) if(is.null(x)) y else x


rhs <- function(f) {
  if (!inherits(f, "formula")) {
    stop("`f` is a not a formula", call. = FALSE)
  }

  if (length(f) != 2) {
    stop("`f` is not a one-sided formula", call. = FALSE)
  }

  f[[2]]
}
