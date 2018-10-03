f <- function(e) {
  g(e)
}

g <- function(e) {
  rlang::trace_back(e)
}
