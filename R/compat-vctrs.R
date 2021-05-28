# Construction ------------------------------------------------------------

data_frame <- function(...) {
  new_data_frame(df_list(...), .class = "tbl")
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


# Slicing -----------------------------------------------------------------

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

vec_init <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}


# Coercion ----------------------------------------------------------------

vec_cast_common <- function(xs, to = NULL) {
  ptype <- vec_ptype_common(xs, ptype = to)
  lapply(xs, vec_cast, to = ptype)
}

vec_cast <- function(x, to) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(to)) {
    return(x)
  }

  if (vec_is_unspecified(x)) {
    return(vec_init(to, vec_size(x)))
  }

  stop_incompatible_cast <- function(x, to) {
    abort(sprintf("Can't convert <%s> to <%s>.",
      .rlang_vctrs_typeof(x),
      .rlang_vctrs_typeof(to)
    ))
  }

  lgl_cast <- function(x, to) {
    lgl_cast_from_num <- function(x) {
      if (any(!x %in% c(0L, 1L))) {
        stop_incompatible_cast(x, to)
      }
      as.logical(x)
    }

    switch(
      .rlang_vctrs_typeof(x),
      logical = x,
      integer = ,
      double = lgl_cast_from_num(x),
      stop_incompatible_cast(x, to)
    )
  }

  int_cast <- function(x, to) {
    int_cast_from_dbl <- function(x) {
      out <- suppressWarnings(as.integer(x))
      if (any((out != x) | xor(is.na(x), is.na(out)))) {
        stop_incompatible_cast(x, double())
      } else {
        out
      }
    }

    switch(
      .rlang_vctrs_typeof(x),
      logical = as.integer(x),
      integer = x,
      double = int_cast_from_dbl(x),
      stop_incompatible_cast(x, to)
    )
  }

  dbl_cast <- function(x, to) {
    switch(
      .rlang_vctrs_typeof(x),
      logical = ,
      integer = as.double(x),
      double = x,
      stop_incompatible_cast(x, to)
    )
  }

  list_cast <- function(x, to) {
    switch(
      .rlang_vctrs_typeof(x),
      list = x,
      stop_incompatible_cast(x, to)
    )
  }

  switch(
    .rlang_vctrs_typeof(to),
    logical = lgl_cast(x, to),
    integer = int_cast(x, to),
    double = dbl_cast(x, to),
    list = list_cast(x, to),
    stop_incompatible_cast(x, to)
  )
}

vec_ptype_common <- function(xs, ptype = NULL) {
  if (!is.null(ptype)) {
    return(vec_ptype(ptype))
  }

  xs <- Filter(function(x) !is.null(x), xs)

  if (length(xs) == 0) {
    NULL
  } else if (length(xs) == 1) {
    vec_ptype(xs[[1]])
  } else {
    xs <- map(xs, vec_ptype)
    Reduce(vec_ptype2, xs)
  }
}

vec_ptype <- function(x) {
  if (vec_is_unspecified(x)) {
    return(.rlang_vctrs_unspecified())
  }

  vec_slice(x, 0)
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

  df_ptype2 <- function(x, y) {
    set_partition <- function(x, y) {
      list(
        both = intersect(x, y),
        only_x = setdiff(x, y),
        only_y = setdiff(y, x)
      )
    }

    # Avoid expensive [.data.frame
    x <- as.list(vec_slice(x, 0))
    y <- as.list(vec_slice(y, 0))

    # Find column types
    names <- set_partition(names(x), names(y))
    if (length(names$both) > 0) {
      common_types <- Map(vec_ptype2, x[names$both], y[names$both])
    } else {
      common_types <- list()
    }
    only_x_types <- x[names$only_x]
    only_y_types <- y[names$only_y]

    # Combine and construct
    out <- c(common_types, only_x_types, only_y_types)
    out <- out[c(names(x), names$only_y)]
    new_data_frame(out)
  }

  rlib_df_ptype2 <- function(x, y) {
    new_data_frame(df_ptype2(x, y), .class = "tbl")
  }
  tib_ptype2 <- function(x, y) {
    new_data_frame(df_ptype2(x, y), .class = c("tbl_df", "tbl"))
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

    base_data_frame = switch(
      .rlang_vctrs_typeof(y),
      base_data_frame = ,
      s3_data_frame = df_ptype2(x, y),
      rlib_data_frame = rlib_df_ptype2(x, y),
      tibble = tib_ptype2(x, y),
      stop_incompatible_type(x, y)
    ),

    rlib_data_frame = switch(
      .rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      s3_data_frame = rlib_df_ptype2(x, y),
      tibble = tib_ptype2(x, y),
      stop_incompatible_type(x, y)
    ),

    tibble = switch(
      .rlang_vctrs_typeof(y),
      base_data_frame = ,
      rlib_data_frame = ,
      tibble = ,
      s3_data_frame = tib_ptype2(x, y),
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
    if (identical(class, "data.frame")) {
      return("base_data_frame")
    }
    if (identical(class, c("tbl", "data.frame"))) {
      return("rlib_data_frame")
    }
    if (identical(class, c("tbl_df", "tbl", "data.frame"))) {
      return("tibble")
    }
    if (inherits(x, "data.frame")) {
      return("s3_data_frame")
    }

    class <- paste0(class, collapse = "/")
    abort(sprintf("Unimplemented class <%s>.", class))
  }

  type <- typeof(x)
  switch(
    type,
    NULL = return("null"),
    logical = if (vec_is_unspecified(x)) {
      return("unspecified")
    } else {
      return(type)
    },
    integer = ,
    double = ,
    character = ,
    raw = ,
    list = return(type)
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
