
make_P <- function(expr, env = parent.frame()) {
  call <- quote(`_P`(x))
  call[[2]] <- expr
  f_new(call, env = env)
}
