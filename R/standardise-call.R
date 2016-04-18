#' Standardise a function call
#'
#' @param call A call
#' @param env Environment in which to look up call value.
#' @export
standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)

  match.call(f, call)
}
