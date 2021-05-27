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
  i <- rep.int(seq_len(vec_size(x)), times)
  vec_slice(x, i)
}

vec_recycle_common <- function(xs, size = NULL) {
  sizes <- vapply(xs, vec_size, integer(1))

  n <- unique(sizes)

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

  to_recycle <- sizes == 1L
  xs[to_recycle] <- lapply(xs[to_recycle], vec_rep, size)

  xs
}

vec_slice <- function(x, i) {
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.numeric(i) || is.character(i))

  if (is.null(x)) {
    return(NULL)
  }

  if (is.data.frame(x)) {
    # We need to be a bit careful to be generic. First empty all
    # columns and expand the df to final size.
    out <- x[i, 0, drop = FALSE]

    # Then fill in with sliced columns
    out[seq_along(x)] <- lapply(x, vec_slice, i)

    # Reset automatic row names to work around `[` weirdness
    if (is.numeric(attr(x, "row.names"))) {
      attr(out, "row.names") <- .set_row_names(nrow(out))
    }

    return(out)
  }

  d <- vec_dims(x)
  if (d == 1) {
    if (is.object(x)) {
      x[i]
    } else {
      x[i, drop = FALSE]
    }
  } else if (d == 2) {
    x[i, , drop = FALSE]
  } else {
    j <- rep(list(quote(expr = )), d - 1)
    eval(as.call(list(quote(`[`), quote(x), quote(i), j, drop = FALSE)))
  }
}
vec_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}

vec_ptype2 <- function(x, y) {
  stop_incompatible_type <- function(x, y) {
    abort(sprintf("Can't combine types <%s> and <%s>.",
      .rlang_vctrs_typeof(x),
      .rlang_vctrs_typeof(y)
    ))
  }

  x_type <- .rlang_vctrs_typeof(x)
  y_type <- .rlang_vctrs_typeof(y)

  if (x_type == "unspecified" && y_type == "unspecified") {
    return(.rlang_vctrs_unspecified())
  }
  if (x_type == "unspecified") {
    return(y)
  }
  if (y_type == "unspecified") {
    return(x)
  }

  ptype <- switch(
    x_type,

    logical = switch(
      y_type,
      logical = x,
      integer = y,
      double = y,
      stop_incompatible_type(x, y)
    ),

    integer = switch(
      .rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = y,
      stop_incompatible_type(x, y)
    ),

    double = switch(
      .rlang_vctrs_typeof(y),
      logical = x,
      integer = x,
      double = x,
      stop_incompatible_type(x, y)
    ),

    character = switch(
      .rlang_vctrs_typeof(y),
      character = x,
      stop_incompatible_type(x, y)
    ),

    list = switch(
      .rlang_vctrs_typeof(y),
      list = x,
      stop_incompatible_type(x, y)
    ),

    stop_incompatible_type(x, y)
  )

  vec_slice(ptype, 0)
}

.rlang_vctrs_typeof <- function(x) {
  if (is.object(x)) {
    class <- class(x)
    if (identical(class, "rlang_unspecified")) {
      return("unspecified")
    }

    class <- paste0(class, collapse = "/")
    abort(sprintf("Unimplemented class <%s>.", class))
  }

  type <- typeof(x)
  switch(
    type,
    logical =
      if (vec_is_unspecified(x)) {
        return("unspecified")
      } else {
        return(type)
      },
    integer = ,
    double = ,
    character = ,
    raw = ,
    list =
      return(type)
  )

  abort(sprintf("Unimplemented type <%s>.", type))
}

vec_is_unspecified <- function(x) {
  !is.object(x) &&
    typeof(x) == "logical" &&
    length(x) &&
    all(vapply(x, identical, logical(1), NA))
}

.rlang_vctrs_unspecified <- function(x = NULL) {
  structure(
    rep(NA, length(x)),
    class = "rlang_unspecified"
  )
}
