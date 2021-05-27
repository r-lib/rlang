data_frame <- function(...) {
  new_data_frame(df_list(...))
}

new_data_frame <- function(.x = list(),
                           ...,
                           .size = NULL,
                           .class = NULL) {
  n_cols <- length(.x)
  if (n_cols != 0 && is.null(names(.x))) {
    abort("Columns must be named.")
  }

  if (is.null(.size)) {
    if (n_cols == 0) {
      .size <- 0
    } else {
      .size <- vec_size(.x[[1]])
    }
  }

  structure(
    .x,
    class = c(.class, "data.frame"),
    row.names = .set_row_names(.size),
    ...
  )
}

df_list <- function(..., .size = NULL) {
  vec_recycle_common(list(...), size = .size)
}

vec_size <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
}

vec_rep <- function(x, times) {
  if (is.data.frame(x)) {
    abort("TODO")
  } else {
    rep.int(x, times)
  }
}

vec_recycle_common <- function(xs, size = NULL) {
  lengths <- vapply(xs, length, integer(1))

  n <- unique(lengths)

  if (length(n) == 1 && is.null(size)) {
    return(xs)
  }
  n <- setdiff(n, 1L)

  ns <- length(n)

  if (ns == 0) {
    if (is.null(size)) {
      return(xs)
    }
  } else if (ns == 1) {
    if (is.null(size)) {
      size <- n
    } else if (ns != size) {
      abort("Inputs can't be recycled to `size`.")
    }
  } else {
    abort("Inputs can't be recycled to a common size.")
  }

  to_recycle <- lengths == 1L
  xs[to_recycle] <- lapply(xs[to_recycle], vec_rep, size)

  xs
}
