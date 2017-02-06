
# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming.

zip <- function(.l) {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
  }

  lapply(fields, function(i) {
    lapply(.l, .subset2, i)
  })
}

vapply_ <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  set_names(out, names(.x))
}
vapply_int <- function(.x, .f, ...) {
  vapply_(.x, .f, integer(1), ...)
}
vapply_lgl <- function(.x, .f, ...) {
  vapply_(.x, .f, logical(1), ...)
}

pluck <- function(.x, .f) {
  lapply(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  vapply_int(.x, `[[`, .f)
}

keep <- function(.x, .f, ...) {
  .x[vapply_lgl(.x, .f, ...)]
}
