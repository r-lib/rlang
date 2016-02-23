#' @useDynLib lazyeval quasiquote_c
quasiquote <- function(f) {
  stopifnot(inherits(f, "formula"), length(f) == 2)

  f[[2]] <- .Call(quasiquote_c, f[[2]], environment(f))
  f
}

quasiquote_ <- function(x, env = parent.frame()) {
  .Call(quasiquote_c, x, env)
}

#' @useDynLib lazyeval is_unquote_c
is_unquote <- function(x) {
  .Call(is_unquote_c, substitute(x))
}
