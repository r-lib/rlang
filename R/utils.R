
substitute_ <- function(x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

get_env <- function(x) {
  if (is_frame(x)) {
    x <- x$env
  } else if (is_formula(x)) {
    x <- environment(x)
  }
  stopifnot(is.environment(x))

  x
}

drop_last <- function(x) {
  x[-length(x)]
}
drop_first <- function(x) {
  x[-1]
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

lapply_around <- function(.x, .neighbour = c("right", "left"), .f, ...) {
  where <- match.arg(.neighbour)
  n <- length(.x)
  out <- vector("list", n)

  if (n == 0) {
    return(.x)
  }

  if (n == 1) {
    out[[1]] <- .f(.x[[1]], arg_missing(), ...)
    return(out)
  }

  if (n > 1 && where == "right") {
    neighbours <- .x[seq(2, n)]
    idx <- seq_len(n - 1)
    out[idx] <- Map(.f, .x[idx], neighbours, ...)
    out[[n]] <- .f(.x[[n]], arg_missing(), ...)
    return(out)
  }

  if (n > 1 && where == "left") {
    neighbours <- .x[seq(1, n - 1)]
    idx <- seq(2, n)
    out[idx] <- Map(.f, .x[idx], neighbours, ...)
    out[[1]] <- .f(.x[[1]], arg_missing(), ...)
    return(out)
  }

  stop("unimplemented")
}
pluck <- function(.x, .f) {
  lapply(.x, `[[`, .f)
}
keep <- function(.x, .f, ...) {
  .x[vapply_lgl(.x, .f, ...)]
}
set_names2 <- function(x, nms = names2(x)) {
  empty <- nms == ""
  nms[empty] <- x[empty]
  names(x) <- nms
  x
}
