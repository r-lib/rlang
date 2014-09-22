
#' @useDynLib lazyeval promise_
promise <- function(expr, env) {
  .Call(promise_, expr, env)
}

#' @useDynLib lazyeval promise_expr_
promise_expr <- function(prom) {
  .Call(promise_expr_, prom)
}

#' @useDynLib lazyeval promise_env_
promise_env <- function(prom) {
  .Call(promise_env_, prom)
}
