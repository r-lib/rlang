#' @useDynLib lazyeval is_unquote_
is_unquote <- function(x) {
  .Call(is_unquote_, substitute(x))
}
