
dots_enumerate_args <- function(dots) {
  i <- 1
  lsp_walk(dots, function(dot) {
    dot_name <- as.symbol(paste0("..", i))
    set_car(dot, dot_name)
    i <<- i + 1
  })

  dots
}
dots_enumerate_argnames <- function(dots) {
  names(dots) <- paste0("..", seq_along(dots))
  dots
}

dots_get <- function(env) {
  env <- get_env(env)
  dots <- substitute(alist(...), env)
  cdr(dots)
}
