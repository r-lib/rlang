
# promise <- function(expr, env) {
#   .Call(promise_, expr, env)
# }

#' @useDynLib lazyeval promise_expr_
promise_expr <- function(prom) {
  .Call(promise_expr_, prom)
}

#' @useDynLib lazyeval promise_env_
promise_env <- function(prom) {
  .Call(promise_env_, prom)
}

#' @export
as.lazy.promise <- function(x, ...) {
  lazy_(promise_expr(x), promise_env(x))
}
