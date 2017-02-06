
# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on.

vapply_ <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  set_names(out, names(.x))
}
vapply_lgl <- function(.x, .f, ...) {
  vapply_(.x, .f, logical(1), ...)
}
vapply_int <- function(.x, .f, ...) {
  vapply_(.x, .f, integer(1), ...)
}
vapply_dbl <- function(.x, .f, ...) {
  vapply_(.x, .f, double(1), ...)
}
vapply_chr <- function(.x, .f, ...) {
  vapply_(.x, .f, character(1), ...)
}
vapply_cpl <- function(.x, .f, ...) {
  vapply_(.x, .f, complex(1), ...)
}

pluck <- function(.x, .f) {
  lapply(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  vapply_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  vapply_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  vapply_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  vapply_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  vapply_cpl(.x, `[[`, .f)
}

probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    vapply_lgl(.x, .p, ...)
  }
}

keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
lapply_if <- function(.x, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- lapply(.x[matches], .f, ...)
  .x
}

flatten <- function(.x) {
  unlist(.x, FALSE, FALSE)
}
compact <- function(.x) {
  Filter(length, .x)
}

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
