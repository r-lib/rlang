
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

#' @useDynLib rlang car_
car <- function(x) {
  .Call(car_, x)
}
#' @useDynLib rlang cdr_
cdr <- function(x) {
  .Call(cdr_, x)
}
#' @useDynLib rlang cadr_
cadr <- function(x) {
  .Call(cadr_, x)
}
#' @useDynLib rlang cddr_
cddr <- function(x) {
  .Call(cddr_, x)
}

#' @useDynLib rlang set_car_
set_car <- function(x, newcar) {
  .Call(set_car_, x, newcar)
}
#' @useDynLib rlang set_cdr_
set_cdr <- function(x, newcdr) {
  .Call(set_cdr_, x, newcdr)
}
#' @useDynLib rlang set_cadr_
set_cadr <- function(x, newcar) {
  .Call(set_cadr_, x, newcar)
}
#' @useDynLib rlang set_cddr_
set_cddr <- function(x, newcdr) {
  .Call(set_cddr_, x, newcdr)
}

#' @useDynLib rlang cons_
cons <- function(car, cdr) {
  .Call(cons_, car, cdr)
}

#' @useDynLib rlang duplicate_
duplicate <- function(x) {
  .Call(duplicate_, x)
}

#' @useDynLib rlang tag_
tag <- function(x) {
  .Call(tag_, x)
}
#' @useDynLib rlang set_tag_
set_tag <- function(x, tag) {
  .Call(set_tag_, x, tag)
}
