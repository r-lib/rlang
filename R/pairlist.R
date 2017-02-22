
lsp_walk <- function(.x, .f, ...) {
  cur <- .x
  while(!is.null(cur)) {
    .f(cur, ...)
    cur <- cdr(cur)
  }
  NULL
}
lsp_walk_nonnull <- function(.x, .f, ...) {
  cur <- .x
  out <- NULL
  while(!is.null(cur) && is.null(out)) {
    out <- .f(cur, ...)
    cur <- cdr(cur)
  }
  out
}
lsp_walk_last <- function(.x, .f, ...) {
  cur <- .x
  while(!is.null(cdr(cur))) {
    cur <- cdr(cur)
  }
  .f(cur, ...)
}

lsp_append <- function(.x, .y) {
  lsp_walk_last(.x, function(l) set_cdr(l, .y))
  .x
}

#' @useDynLib rlang rlang_car
car <- function(x) {
  .Call(rlang_car, x)
}
#' @useDynLib rlang rlang_cdr
cdr <- function(x) {
  .Call(rlang_cdr, x)
}
#' @useDynLib rlang rlang_cadr
cadr <- function(x) {
  .Call(rlang_cadr, x)
}
#' @useDynLib rlang rlang_cddr
cddr <- function(x) {
  .Call(rlang_cddr, x)
}

#' @useDynLib rlang rlang_set_car
set_car <- function(x, newcar) {
  .Call(rlang_set_car, x, newcar)
}
#' @useDynLib rlang rlang_set_cdr
set_cdr <- function(x, newcdr) {
  .Call(rlang_set_cdr, x, newcdr)
}
#' @useDynLib rlang rlang_set_cadr
set_cadr <- function(x, newcar) {
  .Call(rlang_set_cadr, x, newcar)
}
#' @useDynLib rlang rlang_set_cddr
set_cddr <- function(x, newcdr) {
  .Call(rlang_set_cddr, x, newcdr)
}

#' @useDynLib rlang rlang_cons
cons <- function(car, cdr) {
  .Call(rlang_cons, car, cdr)
}

#' @useDynLib rlang rlang_duplicate
#' @useDynLib rlang rlang_shallow_duplicate
duplicate <- function(x, shallow = FALSE) {
  if (shallow) {
    .Call(rlang_shallow_duplicate, x)
  } else {
    .Call(rlang_duplicate, x)
  }
}

#' @useDynLib rlang rlang_tag
tag <- function(x) {
  .Call(rlang_tag, x)
}
#' @useDynLib rlang rlang_set_tag
set_tag <- function(x, tag) {
  .Call(rlang_set_tag, x, tag)
}
