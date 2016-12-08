#' Read R code.
#'
#' These functions parse and transform text into R expressions. This
#' is the first step of interpreting or evaluating a piece of code
#' written by a programmer.
#'
#' \code{read()} returns one expression. If the text contains more
#' than one expression (separated by colons or new lines), an error is
#' issued. On the other hand \code{read_list()} can handle multiple
#' expressions. It always returns a list of expressions (compare to
#' \code{\link[base]{parse}()} which returns an obsolete expression
#' vector). Finally, \code{read_conn()} reads a R \link{connection}.
#'
#' The versions prefixed with \code{f_} return expressions quoted in
#' formulas rather than raw expressions.
#'
#' @param x Text containing expressions to read for \code{read()} and
#'   \code{read_list()}, or a connection to a file for
#'   \code{read_conn()}.
#' @param env The environment for the formulas. Defaults to the
#'   context in which the read function was called. Can be any object
#'   with a \code{as_env()} method.
#' @return \code{read()} returns a formula, \code{read_list()} and
#'   \code{read_conn()} return a list of formulas.
#' @seealso \code{\link[base]{parse}()}
#' @export
#' @examples
#' # read() can read any R expression:
#' read("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#'
#' # A string can contain several expressions separated by ; or \n
#' read_list("NULL; list()\n foo(bar)")
#'
#' # The versions prefixed with f_ return formulas:
#' f_read("foo %>% bar()")
#' f_read_list("1; 2; mtcars")
#'
#' # The env argument is passed to as_env(). It can be e.g. a string
#' # representing a scoped package environment:
#' f_read("identity(letters)", env = env_empty())
#' f_read_list("identity(letters); mtcars", env = "base")
read <- function(x) {
  exprs <- parse(text = read_validate(x))

  n <- length(exprs)
  if (n == 0) {
    stop("No expression to parse", call. = FALSE)
  } else if (n > 1) {
    stop("More than one expression parsed", call. = FALSE)
  }

  exprs[[1]]
}
#' @rdname read
#' @export
read_list <- function(x) {
  exprs <- parse(text = read_validate(x))
  as.list(exprs)
}
#' @rdname read
#' @export
read_conn <- function(x) {
  exprs <- parse(read_validate(x))
  as.list(exprs)
}
read_validate <- function(x) {
  if (!is_scalar_character(x)) {
    stop("Cannot read character vector of length > 1", call. = FALSE)
  }
  x
}

#' @rdname read
#' @export
f_read <- function(x, env = env_caller()) {
  f_new(read(x), env = as_env(env))
}
#' @rdname read
#' @export
f_read_list <- function(x, env = env_caller()) {
  lapply(read_list(x), f_new, env = as_env(env))
}
#' @rdname read
#' @export
f_read_conn <- function(x, env = env_caller()) {
  lapply(read_conn(x), f_new, env = as_env(env))
}
