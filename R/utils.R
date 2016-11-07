"%||%" <- function(x, y) if(is.null(x)) y else x

is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw")
}

is_vector <- function(x) {
  is_atomic(x) || is.list(x)
}

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

substitute_ <- function(x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

#' @useDynLib rlang make_lazy
#' @useDynLib rlang make_lazy_dots
NULL

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
set_names <- function(x, nm = x) {
  stats::setNames(x, nm)
}
vapply_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}
