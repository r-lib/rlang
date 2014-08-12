lazy_eval <- function(x, data = NULL) {
  stopifnot(is.promise(x))

  if (!is.null(data)) {
    eval(x$expr, data, x$env)
  } else {
    eval(x$expr, x$env)
  }
}
