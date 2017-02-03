
substitute_ <- function(x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  call <- substitute(substitute(x, env), list(x = x))
  expr_eval(call)
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
set_names2 <- function(x, nms = names2(x)) {
  empty <- nms == ""
  nms[empty] <- x[empty]
  names(x) <- nms
  x
}
vapply_chr <- function(.x, .f, ...) {
  vapply_(.x, .f, character(1), ...)
}
lapply2 <- function(.x, .y, .f, ...) {
  Map(.f, .x, .y, ...)
}
lapply2_chr <- function(.x, .y, .f, ...) {
  as.vector(lapply2(.x, .y, .f, ...), "character")
}
imap <- function(.x, .f, ...) {
  idx <- names(.x) %||% seq_along(.x)
  out <- Map(.f, idx, .x, ...)
  names(out) <- names(.x)
  out
}
imap_chr <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "character")
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
